## Bencode-RPC

Remote Procedure Call in OCaml, with B-encode and Lwt.

This library allows to communicate through the network (TCP) using
a remote call abstraction. The data is serialized with
[Bencode](http://en.wikipedia.org/wiki/Bencode).

Documentation is available [here](http://cedeela.fr/~simon/software/bencode_rpc/).

### Install

You need OCaml>=4.00, and the libraries `bencode`, `lwt` and `lwt.unix`.

    $ make
    $ make install

### License

Released under the BSD2 license. See `LICENSE`.

### Modules

The library provides the following modules, packed in the module `Bencode_rpc`:

- `NetTcp`: management of TCP connections, and communication of `Bencode`
  objects through sockets
- `RPCServer`: RPC server implementation. Methods (functions from
   Bencode messages to lwt-wrapped Bencode replies, basically) can be
   registered under a name to the server.
- `RPCClient`: connects to a server via TCP, and then allows to call
   remote methods by their name (with a timeout) and get a future
   response.
- `Broadcast`: *untested* broadcasting algorithm for small networks.
- `Lwt_queue`: a message queue for `Lwt`, for internal use
- `Signal`: internal publish/subscribe implementation

### Example

Simple "ping-pong" client-server exchange (can be found in `tests/test1.ml`
with minor modifications).

```ocaml
open Bencode_rpc;;
module B = Bencode;;

(* create a server that listens on some random TCP port *)
let server =
  match RPCServer.create () with
  | Some s -> s
  | None -> failwith "could not start server"
;;

let port = RPCServer.port server;;

(* register a method "ping" on the server. That remote method
   will reply "pong" whenever it receives "ping". *)
RPCServer.register server "ping" (fun _addr_sender msg ->
  match msg with
  | B.String "ping" -> RPCServer.reply (B.String "pong")
  | _ -> RPCServer.error "error: expected \"ping\"");;

let (>>=) = Lwt.(>>=);;

Lwt_main.run (
  (* create a client that tries to connect on the local port of the server *)
  RPCClient.local port
  >>= function
  | None -> failwith "could not connect to server port"
  | Some client ->

  (* call the remote method "ping" with the message "ping" (in Bencode)
     and a timeout of 2 seconds. *)
  RPCClient.call ~timeout:2. client "ping" (B.String "ping")
  >>= function
    | RPCClient.Reply (B.String "pong") ->
      Lwt_io.printl "success!"
    | RPCClient.Reply _ -> failwith "client: bad response"
    | RPCClient.Error msg -> failwith ("client: got error " ^ msg)
    | RPCClient.NoReply -> failwith "client: timeout"
);;
````
