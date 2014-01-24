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

(** {1 Network implementation using TCP sockets} *)

module B = Bencode
module BS = Bencode_streaming

let (>>=) = Lwt.(>>=)

module Address = struct
  type t = Unix.inet_addr * int
    (** Adresses are internet addresses, + port *)

  type addr = t

  let encode (addr, port) =
    B.List [ B.String (Unix.string_of_inet_addr addr); B.Integer port ]

  let decode = function
    | B.List [ B.String addr; B.Integer port ] ->
      begin try
        let addr = Unix.inet_addr_of_string addr in
        addr, port
      with _ -> raise (Invalid_argument "bad address")
      end
    | _ -> raise (Invalid_argument "bad address")

  let eq a b = B.eq (encode a) (encode b)

  let hash a = B.hash (encode a)

  (* change port *)
  let with_port (addr,_) port = (addr, port)

  let to_string (addr, port) =
    Printf.sprintf "%s:%d" (Unix.string_of_inet_addr addr) port

  let to_sockaddr (addr, port) =
    Unix.ADDR_INET (addr, port)

  let of_sockaddr = function
    | Unix.ADDR_INET (a,i) -> a, i
    | _ -> failwith "unexpected Unix address"

  let fmt fmt addr =
    Format.pp_print_string fmt (to_string addr)

  let to_string = to_string

  let local port =
    Unix.inet_addr_any, port

  let by_addr addr port =
    Unix.inet_addr_of_string addr, port

  let by_name host port =
    Lwt_unix.gethostbyname host >>= fun host ->
    if Array.length host.Unix.h_addr_list = 0
      then Lwt.return_none
      else
        let addr = host.Unix.h_addr_list.(0), port in
        Lwt.return (Some addr)

  (** Hashtables on addresses *)
  module Tbl = Hashtbl.Make(struct
    type t = addr
    let equal = eq
    let hash = hash
  end)
end

let call_in time f =
  let fut = Lwt_unix.sleep time in
  Lwt.on_success fut f

let rec _read_bencode ~socket ~decoder ~buf =
  match BS.parse_resume decoder with
  | BS.ParseOk b -> Lwt.return (Some b)
  | BS.ParseError e ->
    Lwt_unix.close socket >>= fun () ->
    Lwt.return_none
  | BS.ParsePartial ->
    (* need to read more input *)
    Lwt_unix.read socket buf 0 (String.length buf) >>= fun n ->
    if n = 0
      then
        Lwt_unix.close socket >>= fun () ->
        Lwt.return_none
      else match BS.parse decoder buf 0 n with
        | BS.ParseError msg ->
          Lwt_unix.close socket >>= fun () ->
          Lwt.return_none
        | BS.ParsePartial ->
          _read_bencode ~socket ~decoder ~buf (* read more *)
        | BS.ParseOk msg ->
          Lwt.return (Some msg)

(* write the given buffer (n bytes remaining). Calls [k]
    when done *)
let rec _write_full ~sock ~buf i n k =
  if n = 0
    then k ()
    else
      Lwt_unix.write sock buf i n >>= fun j ->
      _write_full ~sock ~buf (i+j) (n-j) k

let _log format =
  let b = Buffer.create 15 in
  Printf.kbprintf
    (fun b ->
      Lwt.ignore_result (Lwt_io.printl (Buffer.contents b)))
    b format

(* create a socket with the given address
  TODO: option for ipv6 *)
let mk_socket () =
  Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_STREAM 0

(** {2 Connection to a remote machine} *)

module Connection = struct
  type t = {
    addr : Address.t;
    mutable last_ping : float;  (* timestamp of last activity *)
    mutable alive : bool;
    on_close : unit Lwt_condition.t;
    events : B.t Signal.t;
    decoder : BS.decoder;
    sock : Lwt_unix.file_descr;
    queue : B.t option Lwt_queue.t; (* messages to send *)
  }

  let address t = t.addr

  let close conn =
    if conn.alive then begin
      conn.alive <- false;
      Lwt_condition.broadcast conn.on_close ();
      Lwt_unix.close conn.sock
    end else Lwt.return_unit

  let finalize t =
    Gc.finalise (fun t -> Lwt.ignore_result (close t)) t

  (* read messages from the remote end *)
  let _listen_incoming conn =
    let buf = String.make 256 ' ' in
    let rec read () =
      if not conn.alive then Lwt.return_unit else
      _read_bencode ~socket:conn.sock ~decoder:conn.decoder ~buf >>=
      function
      | None -> close conn
      | Some b ->
        Signal.send conn.events b;
        read ()
    in
    read()

  (* read messages to send to the remote end *)
  let rec _listen_queue conn =
    if not conn.alive then Lwt.return_unit else
    Lwt_queue.pop conn.queue >>= function
    | None -> close conn
    | Some b ->
      let s = BS.to_string b in
      _write_full ~sock:conn.sock ~buf:s 0 (String.length s)
        (fun () -> _listen_queue conn)

  let _create addr sock =
    let conn = {
      addr;
      sock;
      alive = true;
      on_close = Lwt_condition.create ();
      last_ping = Unix.gettimeofday();
      decoder = BS.mk_decoder ();
      events = Signal.create ();
      queue = Lwt_queue.create ();
    } in
    Lwt.ignore_result (_listen_incoming conn);
    Lwt.ignore_result (_listen_queue conn);
    conn

  let try_open addr =
    (* connect to the remote address *)
    let sock = mk_socket () in
    Lwt.catch
      (fun () ->
        Lwt_unix.connect sock (Address.to_sockaddr addr) >>= fun () ->
        let conn = _create addr sock in
        Lwt.return (Some conn))
      (fun _ -> Lwt.return_none)

  let send conn msg =
    Lwt_queue.push conn.queue (Some msg)

  let is_closed conn = not conn.alive

  let events conn = conn.events

  let wait conn = Lwt_condition.wait conn.on_close

  let local port = try_open (Address.local port)

  let by_host h port = try_open (Address.by_addr h port)

  let by_name name port =
    Address.by_name name port >>= function
    | None -> Lwt.return_none
    | Some addr -> try_open addr

  let of_sock sock addr =
    _create addr sock
end

(** {2 Connections from remote machines} *)

module Server = struct
  type receive_ev = {
    rcv_conn : Connection.t;
    rcv_msg : Bencode.t;
    rcv_addr : Address.t;
  } (** Content of a "receive" event *)

  let msg ev = ev.rcv_msg

  let reply ev b = Connection.send ev.rcv_conn b

  let addr ev = ev.rcv_addr

  type event =
    | Receive of receive_ev
    | Stop  (* stop the DHT *)

  type t = {
    events : event Signal.t;
    port : int;
    clients : Connection.t Address.Tbl.t;
    mutable listen_thread : unit Lwt.t option;
    mutable stop : bool;
    on_stop : unit Lwt_condition.t;
  } (** Network layer that uses sockets *)

  let events t = t.events

  let port t = t.port

  let stop t =
    if not t.stop then begin
      t.stop <- true;
      Lwt_condition.broadcast t.on_stop ()
    end

  let wait t =
    if t.stop
      then Lwt.return_unit
      else Lwt_condition.wait t.on_stop

  (* handle given new client *)
  let _handle_client t socket addr =
    (* create connection to client *)
    let conn = Connection.of_sock socket addr in
    assert (not (Address.Tbl.mem t.clients addr));
    Address.Tbl.add t.clients addr conn;
    (* forward received messages *)
    Signal.on (Connection.events conn)
      (fun msg ->
        _log "received %s from client" (BS.pretty_to_str msg);
        let ev = {rcv_conn=conn; rcv_msg=msg; rcv_addr=addr;} in
        Signal.send t.events (Receive ev);
        true);
    (* upon closure of connection, react *)
    Lwt.on_success (Connection.wait conn)
      (fun () ->
        Address.Tbl.remove t.clients (Connection.address conn));
    Lwt.return_unit

  (* main thread that listens to incoming connections *)
  let _listen t =
    let addr = Address.local t.port in
    let s = mk_socket () in
    Lwt_unix.bind s (Address.to_sockaddr addr);
    Lwt_unix.listen s 1024;
    let rec accept () =
      Lwt.pick
        [ (Lwt_unix.accept s >>= fun (s', addr') ->
          (* re-extract address! *)
          let addr' = match addr' with
            | Unix.ADDR_UNIX _ -> assert false
            | Unix.ADDR_INET (a,i) -> a, i
          in
          _handle_client t s' addr')
        ; Lwt_condition.wait t.on_stop  (* stop! *)
        ]
      >>= fun () ->
      if not t.stop
        then accept ()  (* accept next connection *)
        else Lwt.return_unit
    in
    accept ()

  let rec create ?(retry=3) ?port () =
    (* random port if none is specified *)
    let port' = match port with
      | Some p -> p
      | None -> (Random.int 5000) + 1024
    in
    let t = {
      events = Signal.create ();
      port=port';
      listen_thread = None;
      clients = Address.Tbl.create 15;
      stop = false;
      on_stop = Lwt_condition.create ();
    } in
    try
      t.listen_thread <- Some (_listen t);
      Some t
    with Unix.Unix_error _ ->
      (* should we try another port? *)
      if retry > 0 && port = None
        then create ~retry:(retry-1) ?port ()
        else None
end
