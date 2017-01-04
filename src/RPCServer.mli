
(* This file is free software, part of bencode_rpc. See file "license" for more details. *)

(** {1 RPC server} *)

type address = NetTcp.Address.t

type result =
  | NoReply
  | Reply of Bencode.t
  | Error of string

type method_ = address -> Bencode.t -> result Lwt.t
  (** A method can return a result, given an argument (a {!Bencode.t} value)
      and the address from the caller *)

type t
  (** A RPC server, exposing a bunch of methods *)

val port : t -> int
  (** Port the system listens on *)

val wait : t -> unit Lwt.t
  (** Wait for the server to stop *)

val of_server : NetTcp.Server.t -> t
  (** Create an instance of the RPC system, which can send and receive
      remote function calls using the {!Net_tcp.Server.t} instance. *)

val create : ?port:int -> ?retry:int -> unit -> t option
  (** Calls {!Net_tcp.Server.create} with the given optional arguments,
      and then build a RPC server on top of it *)

val stop : t -> unit
  (** Disable all threads and active processes *)

val register : t -> string -> method_ -> unit
  (** [register rpc name f] registers [f] under the given [name]. Calls
      to [name] will be handled to [f].
      @raise Failure when the name is already taken. *)

val fmt : Format.formatter -> t -> unit

(** {3 Helpers to build methods} *)

val reply : Bencode.t -> result Lwt.t

val no_reply : result Lwt.t

val error : String.t -> result Lwt.t
