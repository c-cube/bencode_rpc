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

type result =
  | NoReply
  | Reply of Bencode.t
  | Error of string

module B = Bencode

let (>>=) = Lwt.(>>=)

type t = {
  conn : Net.Connection.t;
  mutable count : int;  (* message id *)
  period : float;
  callbacks : (int, (float * result Lwt.u)) Hashtbl.t;
}

let connection proxy = proxy.conn 

let address proxy = Net.Connection.address proxy.conn

let is_alive proxy = not (Net.Connection.is_closed proxy.conn)

let close proxy =
  if is_alive proxy then begin
    Lwt.ignore_result (Net.Connection.close proxy.conn);
    Hashtbl.iter
      (fun _ (_, promise) -> Lwt.wakeup promise (Error "closed connection"))
      proxy.callbacks
  end

(* check whether some callbacks timed out *)
let _check_timeouts proxy =
  let to_remove = ref [] in
  let now = Unix.gettimeofday () in
  (* find callbacks that have expired *)
  Hashtbl.iter
    (fun i (ttl, promise) ->
      if ttl < now
        then to_remove := (i, promise) :: !to_remove)
    proxy.callbacks;
  (* remove all such callbacks *)
  List.iter
    (fun (i, promise) ->
      Hashtbl.remove proxy.callbacks i;
      Lwt.wakeup promise (Error "timeout"))
    !to_remove;
  ()

(* wait some time, then check timeouts and loop *)
let rec _poll proxy =
  if is_alive proxy
    then Net.call_in proxy.period
      (fun () ->
        _check_timeouts proxy;
        _poll proxy)

(* handle received messages *)
let _handle_incoming proxy =
  Signal.on (Net.Connection.events proxy.conn)
    (fun msg ->
      begin match msg with
      | B.L [ B.S "reply"; B.I i; arg ] ->
        begin try
          (* find which promise corresponds to this reply *)
          let _, promise = Hashtbl.find proxy.callbacks i in
          (*Util.debug "got reply for %d: %s" i (B.to_string (B.L args));*)
          Hashtbl.remove proxy.callbacks i;
          Lwt.wakeup promise (Reply arg);
        with Not_found -> ()
        end
      | B.L [ B.S "error" ; B.I i ; B.S errmsg ] ->
        begin try
          (* find which promise corresponds to this reply *)
          let _, promise = Hashtbl.find proxy.callbacks i in
          Hashtbl.remove proxy.callbacks i;
          Lwt.wakeup promise (Error errmsg);
        with Not_found -> ()
        end
      | _ -> ()
      end;
      true)

let of_conn ?(period=5.) conn =
  let proxy = {
    conn;
    count = 0;
    period;
    callbacks = Hashtbl.create 11;
  } in
  _poll proxy;  (* check timeouts *)
  _handle_incoming proxy; (* react to messages *)
  proxy

let call ?(timeout=20.) proxy name arg =
  assert (timeout > 0.);
  if not (is_alive proxy) then failwith "cannot call, proxy closed";
  (* future for the answer, put it in hashtable *)
  let future, promise = Lwt.wait () in
  let n = proxy.count in
  proxy.count <- n + 1;
  let ttl = Unix.gettimeofday() +. timeout in
  Hashtbl.add proxy.callbacks n (ttl, promise);
  (* send message wrapped in metadata *)
  let msg' = B.L [ B.S "call"; B.I n; B.S name; arg] in
  Net.Connection.send proxy.conn msg';
  future

let call_ignore proxy name arg =
  (* send message wrapped in metadata *)
  let msg' = B.L [ B.S "ntfy"; B.S name; arg ] in
  Net.Connection.send proxy.conn msg';
  ()

let connect ?period addr =
  Net.Connection.try_open addr >>= function
  | None -> Lwt.return_none
  | Some c -> Lwt.return (Some (of_conn ?period c))

let by_host ?period a i =
  Net.Connection.by_host a i >>= function
  | None -> Lwt.return_none
  | Some c -> Lwt.return (Some (of_conn ?period c))

let local ?period i =
  Net.Connection.local i >>= function
  | None -> Lwt.return_none
  | Some c -> Lwt.return (Some (of_conn ?period c))

let by_name ?period a i =
  Net.Connection.by_name a i >>= function
  | None -> Lwt.return_none
  | Some c -> Lwt.return (Some (of_conn ?period c))
