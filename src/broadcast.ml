
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
WARRANTIES OF MERCHANTAB.ListITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR B.StringINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIAB.ListITY, WHETHER IN CONTRACT, STRICT LIAB.ListITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIB.ListITY OF SUCH DAMAGE.
*)

(** {1 Simple broadcast} *)

module Net = NetTcp
module B = Bencode

let (>>=) = Lwt.(>>=)

type address = NetTcp.Address.t

module MsgHashtbl = Hashtbl.Make(struct
  type t = B.t
  let equal = B.eq
  let hash = B.hash
end)

type event =
  | Receive of Bencode.t
  | NewNeighbor of address

type t = {
  cache_timeout : float;
  rpc : RPCServer.t;
  cache : float MsgHashtbl.t;  (* message -> last time received *)
  neighbors : RPCClient.t Net.Address.Tbl.t;  (* set of neighbors *)
  events : event Signal.t;
}

let _add_neighbor t proxy =
  let addr = RPCClient.address proxy in
  Net.Address.Tbl.replace t.neighbors addr proxy

let _remove_neighbor t addr =
  Net.Address.Tbl.remove t.neighbors addr

(* attempt to connect to a remote node *)
let _try_connect t addr =
  let fut = RPCClient.connect addr in
  Lwt.on_success fut
    (function
      | None -> ()
      | Some proxy -> _add_neighbor t proxy)

let rec _cleanup t () =
  let now = Unix.gettimeofday () in
  let to_remove = MsgHashtbl.fold
    (fun msg time acc ->
      if time +. t.cache_timeout < now
        then msg :: acc   (* too old, remove *)
        else acc)
    t.cache []
  in
  List.iter (fun msg -> MsgHashtbl.remove t.cache msg) to_remove;
  (* do it again soon *)
  Net.call_in (t.cache_timeout /. 2.) (_cleanup t);
  ()

let _broadcast t msg =
  Net.Address.Tbl.iter
    (fun _ proxy -> RPCClient.call_ignore proxy "broadcast" msg)
    t.neighbors;
  ()

let _dispatch t =
  (* broadcast message *)
  RPCServer.register t.rpc "broadcast"
    (fun _ msg ->
      if not (MsgHashtbl.mem t.cache msg) then begin
        MsgHashtbl.add t.cache msg (Unix.gettimeofday());
        (* deliver message *)
        Signal.send t.events (Receive msg);
        (* re-broadcast *)
        _broadcast t msg
      end;
      RPCServer.no_reply);
  (* new connection *)
  RPCServer.register t.rpc "hello"
    (fun addr msg ->
      match msg with
      | B.Integer port ->
        (* protocol: send the port on which remote node listens *)
        let addr = Net.Address.with_port addr port in
        _try_connect t addr;
        RPCServer.reply (B.String "world")
      | _ ->
        RPCServer.error"expected port");
  (* keepalive *)
  RPCServer.register t.rpc "ping"
    (fun _ _ -> RPCServer.reply (B.String "pong"));
  ()

let create ?(cache_timeout=60.) rpc =
  let t = {
    cache_timeout;
    rpc;
    cache = MsgHashtbl.create 127;
    neighbors = Net.Address.Tbl.create 7;
    events = Signal.create ();
  } in
  (* handle incoming messages *)
  _dispatch t;
  (* call _cleanup regularly *)
  Net.call_in cache_timeout (_cleanup t);
  t

let broadcast t msg = _broadcast t msg

let connect t addr =
  (* connect to addr *)
  RPCClient.connect addr >>= function
  | None -> Lwt.return_false
  | Some proxy ->
    let port = RPCServer.port t.rpc in
    (* send my port *)
    let msg = B.Integer port in
    RPCClient.call proxy "hello" msg >>= function
    | RPCClient.Reply (B.String "world") ->
      _add_neighbor t proxy;
      Lwt.return_true
    | _ -> Lwt.return_false

let neighbors t =
  Net.Address.Tbl.fold
    (fun addr _ acc -> addr :: acc)
    t.neighbors []

let events t = t.events

let recv t =
  let fut, wake = Lwt.wait () in
  Signal.on
    t.events
    (fun e -> Lwt.wakeup wake e; false);
  fut
