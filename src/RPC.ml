(*
Copyright (c) 2013, Simon Cruanes
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

Redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.  Redistributions in binary
form must reproduce the above copyright notice, this list of conditions and the
following disclaimer in the documentation and/or other materials provided with
the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

(** {6 RPC} *)

module Net = Net_tcp

type address = Net.Address.t 

type reply_tag = {
  rt_count : int;
  rt_address : address;
} (* stores information necessary to reply to a message *)

type t = {
  net : Net.t;
  frequency : float;
  mutable count : int;
  mutable stop : bool;
  callbacks : (int, (float * result Lwt.u)) Hashtbl.t;
  methods : (string, method_) Hashtbl.t;
} (** The RPC system *)

and result =
  | NoReply
  | Reply of Bencode.t list
  | Error of string

and method_ = Bencode.t list -> result Lwt.t
  (** A method can return a result, given a list of arguments *)

module B = Bencode

let (>>=) = Lwt.(>>=)

(* check whether some callbacks timed out *)
let check_timeouts rpc =
  let to_remove = ref [] in
  let now = Unix.gettimeofday () in
  (* find callbacks that have expired *)
  Hashtbl.iter
    (fun i (ttl, promise) ->
      if ttl < now
        then to_remove := (i, promise) :: !to_remove)
    rpc.callbacks;
  (* remove all such callbacks *)
  List.iter
    (fun (i, promise) ->
      Hashtbl.remove rpc.callbacks i;
      Lwt.wakeup promise (Error "timeout"))
    !to_remove;
  ()

(* wait some time, then check timeouts and loop *)
let rec poll rpc =
  if not rpc.stop
    then Net.call_in rpc.frequency
      (fun () ->
        check_timeouts rpc;
        poll rpc)

let stop rpc =
  rpc.stop <- true

(* handle network event *)
let handle_event rpc ev = match ev with
  | Net.Stop -> stop rpc; false
  | Net.Receive (addr, msg) ->
    begin match msg with
    | B.L ( B.S "ntfy" :: B.S name :: args )  ->
      begin try
        let method_ = Hashtbl.find rpc.methods name in
        Lwt.ignore_result (method_ args)
      with Not_found -> ()
      end
    | B.L ( B.S "call" :: B.I i :: B.S name :: args ) ->
      begin try
        let method_ = Hashtbl.find rpc.methods name in
        (*Util.debug "method %s called with args %s" name (B.to_string (B.L args));*)
        let fut = method_ args in
        Lwt.on_any fut
          (function 
            | NoReply -> ()
            | Reply args' ->
              let msg' = B.L ( B.S "reply" :: B.I i :: args' ) in
              Net.send rpc.net addr msg';
            | Error errmsg ->
              let msg' = B.L [ B.S "error" ; B.I i ; B.S errmsg ] in
              Net.send rpc.net addr msg')
          (fun exn ->
            let errmsg = Printexc.to_string exn in
            let msg' = B.L [ B.S "error" ; B.I i ; B.S errmsg ] in
            Net.send rpc.net addr msg')
      with Not_found ->
        let msg' = B.L [ B.S "error" ; B.I i ; B.S "method not found" ] in
        Net.send rpc.net addr msg'
      end
    | B.L ( B.S "reply" :: B.I i :: args) ->
      begin try
        (* find which promise corresponds to this reply *)
        let _, promise = Hashtbl.find rpc.callbacks i in
        (*Util.debug "got reply for %d: %s" i (B.to_string (B.L args));*)
        Hashtbl.remove rpc.callbacks i;
        Lwt.wakeup promise (Reply args);
      with Not_found -> ()
      end
    | B.L [ B.S "error" ; B.I i ; B.S errmsg ] ->
      begin try
        (* find which promise corresponds to this reply *)
        let _, promise = Hashtbl.find rpc.callbacks i in
        Hashtbl.remove rpc.callbacks i;
        Lwt.wakeup promise (Error errmsg);
      with Not_found -> ()
      end
    | _ ->
      Printf.eprintf "ill-formed RPC message: %s\n" (B.pretty_to_str msg)
    end;
    true

(* create a new RPC system *)
let create ?(frequency=2.0) net =
  let rpc = {
    net;
    frequency;
    count = 1;
    stop = false;
    callbacks = Hashtbl.create 15;
    methods = Hashtbl.create 15;
  } in
  poll rpc;
  Signal.on
    (Net.events rpc.net)
    (fun e -> handle_event rpc e);
  rpc

let register rpc name method_ =
  if Hashtbl.mem rpc.methods name
    then failwith ("remote method " ^ name ^ " already defined");
  (*Util.debug "register RPC method %s" name;*)
  Hashtbl.add rpc.methods name method_

let call ?timeout rpc addr name args =
  if rpc.stop then failwith "RPC system stopped";
  (* future for the answer, put it in hashtable *)
  let future, promise = Lwt.wait () in
  let n = rpc.count in
  rpc.count <- n + 1;
  let ttl = match timeout with
    | None -> infinity
    | Some t -> (assert (t> 0.); Unix.gettimeofday () +. t)
  in
  Hashtbl.add rpc.callbacks n (ttl, promise);
  (* send message wrapped in metadata *)
  let msg' = B.L ( B.S "call" :: B.I n :: B.S name :: args) in
  Net.send rpc.net addr msg';
  future

let call_ignore rpc addr name args =
  (* send message wrapped in metadata *)
  let msg' = B.L ( B.S "ntfy" :: B.S name :: args) in
  Net.send rpc.net addr msg';
  ()
