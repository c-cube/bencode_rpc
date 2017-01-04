

(* This file is free software, part of bencode_rpc. See file "license" for more details. *)

(** {1 Simple broadcast} *)

module Net = Net_tcp
module B = Bencode

let (>>=) = Lwt.(>>=)

type address = Net_tcp.Address.t

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
  rpc : RPC_server.t;
  cache : float MsgHashtbl.t;  (* message -> last time received *)
  neighbors : RPC_client.t Net.Address.Tbl.t;  (* set of neighbors *)
  events : event Signal.t;
}

let _add_neighbor t proxy =
  let addr = RPC_client.address proxy in
  Net.Address.Tbl.replace t.neighbors addr proxy

let _remove_neighbor t addr =
  Net.Address.Tbl.remove t.neighbors addr

(* attempt to connect to a remote node *)
let _try_connect t addr =
  let fut = RPC_client.connect addr in
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
    (fun _ proxy -> RPC_client.call_ignore proxy "broadcast" msg)
    t.neighbors;
  ()

let _dispatch t =
  (* broadcast message *)
  RPC_server.register t.rpc "broadcast"
    (fun _ msg ->
      if not (MsgHashtbl.mem t.cache msg) then begin
        MsgHashtbl.add t.cache msg (Unix.gettimeofday());
        (* deliver message *)
        Signal.send t.events (Receive msg);
        (* re-broadcast *)
        _broadcast t msg
      end;
      RPC_server.no_reply);
  (* new connection *)
  RPC_server.register t.rpc "hello"
    (fun addr msg ->
      match msg with
      | B.Integer port ->
        (* protocol: send the port on which remote node listens *)
        let addr = Net.Address.with_port addr port in
        _try_connect t addr;
        RPC_server.reply (B.String "world")
      | _ ->
        RPC_server.error"expected port");
  (* keepalive *)
  RPC_server.register t.rpc "ping"
    (fun _ _ -> RPC_server.reply (B.String "pong"));
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
  RPC_client.connect addr >>= function
  | None -> Lwt.return_false
  | Some proxy ->
    let port = RPC_server.port t.rpc in
    (* send my port *)
    let msg = B.Integer port in
    RPC_client.call proxy "hello" msg >>= function
    | RPC_client.Reply (B.String "world") ->
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
