
(*
copyright (c) 2013, simon cruanes
all rights reserved.

redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.  redistributions in binary
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

(** {1 RPC server} *)

module B = Bencode
module Net = Net_tcp

let (>>=) = Lwt.(>>=)

type address = Net_tcp.Address.t

type result =
  | NoReply
  | Reply of Bencode.t
  | Error of string

type method_ = address -> Bencode.t -> result Lwt.t
  (** A method can return a result, given an argument (a {!Bencode.t} value)
      and the address from the caller *)

type reply_tag = {
  rt_count : int;
  rt_ev : Net.Server.receive_ev;
} (* stores information necessary to reply to a message *)

type t = {
  net : Net.Server.t;
  methods : (string, method_) Hashtbl.t;
} (** The RPC system *)

let port rpc = Net.Server.port rpc.net

let wait rpc = Net.Server.wait rpc.net

let stop rpc = Net.Server.stop rpc.net

(* handle network event *)
let _handle_incoming rpc =
  Signal.on (Net.Server.events rpc.net)
    (fun ev -> match ev with
    | Net.Server.Stop -> false
    | Net.Server.Receive rcv_ev ->
      let addr = Net.Server.addr rcv_ev in
      begin match Net.Server.msg rcv_ev with
      | B.L [ B.S "ntfy"; B.S name; arg ]  ->
        (* notification *)
        begin try
          let method_ = Hashtbl.find rpc.methods name in
          Lwt.ignore_result (method_ addr arg)
        with Not_found -> ()
        end
      | B.L [ B.S "call"; B.I i; B.S name; arg ] ->
        (* call expecting answer *)
        begin try
          let method_ = Hashtbl.find rpc.methods name in
          (*Util.debug "method %s called with args %s" name (B.to_string (B.L args));*)
          let fut = method_ addr arg in
          Lwt.on_any fut
            (function 
              | NoReply -> ()
              | Reply arg' ->
                let msg' = B.L [ B.S "reply" ; B.I i; arg' ] in
                Net.Server.reply rcv_ev msg';
              | Error errmsg ->
                let msg' = B.L [ B.S "error" ; B.I i ; B.S errmsg ] in
                Net.Server.reply rcv_ev msg')
            (fun exn ->
              let errmsg = Printexc.to_string exn in
              let msg' = B.L [ B.S "error" ; B.I i ; B.S errmsg ] in
              Net.Server.reply rcv_ev msg')
        with Not_found ->
          let msg' = B.L [ B.S "error" ; B.I i ; B.S "method not found" ] in
          Net.Server.reply rcv_ev msg'
        end
      | _ -> ()
      end;
      true)

(* create a new RPC system *)
let create net =
  let rpc = {
  net;
  methods = Hashtbl.create 15;
  } in
  _handle_incoming rpc;
  rpc

let register rpc name method_ =
  if Hashtbl.mem rpc.methods name
    then failwith ("remote method " ^ name ^ " already defined");
  (*Util.debug "register RPC method %s" name;*)
  Hashtbl.add rpc.methods name method_

let reply b = Lwt.return (Reply b)

let no_reply = Lwt.return NoReply

let error s = Lwt.return (Error s)