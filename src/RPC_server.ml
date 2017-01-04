
(* This file is free software, part of bencode_rpc. See file "license" for more details. *)

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
           | B.List [ B.String "ntfy"; B.String name; arg ]  ->
             (* notification *)
             begin try
                 let method_ = Hashtbl.find rpc.methods name in
                 Lwt.ignore_result (method_ addr arg)
               with Not_found -> ()
             end
           | B.List [ B.String "call"; B.Integer i; B.String name; arg ] ->
             (* call expecting answer *)
             begin try
                 let method_ = Hashtbl.find rpc.methods name in
                 (*Util.debug "method %s called with args %s" name (B.to_string (B.L args));*)
                 let fut = method_ addr arg in
                 Lwt.on_any fut
                   (function
                     | NoReply -> ()
                     | Reply arg' ->
                       let msg' = B.List [ B.String "reply" ; B.Integer i; arg' ] in
                       Net.Server.reply rcv_ev msg';
                     | Error errmsg ->
                       let msg' = B.List [ B.String "error" ;
                                           B.Integer i ; B.String errmsg ] in
                       Net.Server.reply rcv_ev msg')
                   (fun exn ->
                      let errmsg = Printexc.to_string exn in
                      let msg' = B.List [ B.String "error" ;
                                          B.Integer i ; B.String errmsg ] in
                      Net.Server.reply rcv_ev msg')
               with Not_found ->
                 let msg' = B.List [ B.String "error" ; B.Integer i ;
                                     B.String "method not found" ] in
                 Net.Server.reply rcv_ev msg'
             end
           | _ -> ()
         end;
         true)

(* create a new RPC system *)
let of_server net =
  let rpc = {
    net;
    methods = Hashtbl.create 15;
  } in
  _handle_incoming rpc;
  rpc

let create ?port ?retry () =
  match Net.Server.create ?port ?retry () with
    | None -> None
    | Some net -> Some (of_server net)

let register rpc name method_ =
  if Hashtbl.mem rpc.methods name
  then failwith ("remote method " ^ name ^ " already defined");
  (*Util.debug "register RPC method %s" name;*)
  Hashtbl.add rpc.methods name method_

let fmt fmt rpc =
  Format.fprintf fmt "<RPC_server on %d>" (port rpc)

let reply b = Lwt.return (Reply b)

let no_reply = Lwt.return NoReply

let error s = Lwt.return (Error s)
