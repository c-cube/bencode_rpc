#!/usr/bin/env ocaml
#use "tests/.common.ml";;

(* test with one server and 2 clients *)

open Bencode_rpc;;

let server =
  match RPCServer.create () with
  | Some s -> s
  | None -> fail "could not start server"
;;

let port = RPCServer.port server;;

let (>>=) = Lwt.(>>=);;

RPCServer.register server "echo" (fun _ msg ->
  match msg with
  | Bencode.String s ->
      (Lwt_unix.sleep 1. >>= fun () ->
      RPCServer.reply (Bencode.String ("got:" ^ s)))
  | _ -> RPCServer.error "error: expected string");;

let () = Lwt_main.run (
  RPCClient.local port >>= fun c1 ->
  RPCClient.local port >>= fun c2 ->
  let c1, c2 = match c1, c2 with
    | None, _
    | _, None -> fail "could not create clients"
    | Some c1, Some c2 -> c1, c2
  in

  RPCClient.call ~timeout:1.2 c1 "echo" (Bencode.String "1") >>= fun r1 ->
  RPCClient.call ~timeout:1.2 c2 "echo" (Bencode.String "2") >>= fun r2 ->

  match r1, r2 with
    | RPCClient.Reply (Bencode.String "got:1"),
      RPCClient.Reply (Bencode.String "got:2") -> ok ()
    | RPCClient.NoReply, _
    | _, RPCClient.NoReply -> fail "client: timeout"
    | _ -> fail "error"
);;
