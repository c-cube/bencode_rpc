#!/usr/bin/env ocaml
#use "tests/.common.ml";;

open Bencode_rpc;;

let server =
  match RPCServer.create () with
  | Some s -> s
  | None -> fail "could not start server"
;;

let port = RPCServer.port server;;

RPCServer.register server "ping" (fun _ msg ->
  match msg with
  | Bencode.String "ping" -> RPCServer.reply (Bencode.String "pong")
  | _ -> RPCServer.error "error: expected \"ping\"");;

let (>>=) = Lwt.(>>=);;

let () = Lwt_main.run (
  RPCClient.local port
  >>= function
  | None -> fail "could not connect to server port"
  | Some client ->

  let response = RPCClient.call ~timeout:2. client "ping" (Bencode.String "ping") in

  response
  >>= function
    | RPCClient.Reply (Bencode.String "pong") -> ok ()
    | RPCClient.Reply _ -> fail "client: bad response"
    | RPCClient.Error msg -> fail ("client: got errror " ^ msg)
    | RPCClient.NoReply -> fail "client: timeout"
);;
