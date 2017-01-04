
(* This file is free software, part of bencode_rpc. See file "license" for more details. *)

(** {6 RPC} *)

module Net = NetTcp

module B = Bencode

type address = Net.Address.t 

type 'a result =
  | NoReply
  | Reply of 'a
  | Error of string

module Result = struct
  type +'a t = 'a result
  let return x = Reply x
  let fail s = Error s
  let none = NoReply
  let (>>=) x f = match x with
    | NoReply -> NoReply
    | Error s -> Error s
    | Reply x -> f x
end

let (>>=) = Lwt.(>>=)

type t = {
  conn : Net.Connection.t;
  mutable count : int;  (* message id *)
  period : float;
  callbacks : (int, (float * Bencode.t result Lwt.u)) Hashtbl.t;
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
  (* find callbacks that have expired
    FIXME write a more scalable algorithm... *)
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
      | B.List [ B.String "reply"; B.Integer i; arg ] ->
        begin try
          (* find which promise corresponds to this reply *)
          let _, promise = Hashtbl.find proxy.callbacks i in
          (*Util.debug "got reply for %d: %s" i (B.to_string (B.L args));*)
          Hashtbl.remove proxy.callbacks i;
          Lwt.wakeup promise (Reply arg);
        with Not_found -> ()
        end
      | B.List [ B.String "error" ; B.Integer i ; B.String errmsg ] ->
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
  let msg' = B.List [ B.String "call"; B.Integer n; B.String name; arg] in
  Net.Connection.send proxy.conn msg';
  future

let call_ignore proxy name arg =
  (* send message wrapped in metadata *)
  let msg' = B.List [ B.String "ntfy"; B.String name; arg ] in
  Net.Connection.send proxy.conn msg';
  ()

module Typed = struct
  type ('a,'b) method_ = {
    name : string;
    encode : 'a -> Bencode.t;
    decode : Bencode.t -> 'b;
  }

  let create ~name ~encode ~decode =
    {name; encode; decode; }

  let call ?timeout rpc method_ param =
    let b = method_.encode param in
    let res = call ?timeout rpc method_.name b in
    Lwt.map
      (function
      | NoReply -> NoReply
      | Error s -> Error s
      | Reply b ->
        try
          let result = method_.decode b in
          Reply result
        with e -> Error ( "could not decode result " ^
                          Bencode_streaming.to_string b)
      ) res

  let call_ignore rpc method_ param =
    call_ignore rpc method_.name (method_.encode param)
end

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

let fmt fmt rpc =
  Format.fprintf fmt "<RPCClient on %a>" Net.Address.fmt (address rpc)
