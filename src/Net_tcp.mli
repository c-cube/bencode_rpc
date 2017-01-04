
(* This file is free software, part of bencode_rpc. See file "license" for more details. *)

(** {1 Network implementation using TCP sockets} *)

module Address : sig
  type t = Unix.inet_addr * int
    (** A network address (IP:Port, typically) *)

  val encode : t -> Bencode.t
    (** Serialize the address *)

  val decode : Bencode.t -> t
    (** May raise {! Invalid_argument} *)

  val eq : t -> t -> bool
    (** Equality of adresses *)

  val hash : t -> int

  val with_port : t -> int -> t
    (** Same address with a different port *)

  val local : int -> t
    (** Local adress, with the given port *)

  val by_addr : string -> int -> t
    (** Address with IPv4 + port *)

  val by_name : string -> int -> t option Lwt.t
    (** DNS lookup to find an IP, and make an adress out of
        it and the given port number *)

  val fmt : Format.formatter -> t -> unit
  val to_string : t -> string

  module Tbl : Hashtbl.S with type key = t
end

val call_in : float -> (unit -> unit) -> unit
  (** Call the function in the given amount of seconds *)

(** {2 Connection to a remote machine} *)

module Connection : sig
  type t
    (** Connection to a remote process or machine *)

  val address : t -> Address.t

  val try_open : Address.t -> t option Lwt.t
    (** Connection to a remote machine or process by its address.
        @return a connection or None *)

  val close : t -> unit Lwt.t
    (** Close connection *)

  val is_closed : t -> bool
    (** Connection closed? *)

  val wait : t -> unit Lwt.t
    (** Wait for the connection to close *)

  val finalize : t -> unit
    (** Add a finalizer on the connection so that it's closed when it's
        garbage collected *)

  val send : t -> Bencode.t -> unit
    (** Send a message to the remote machine. The message will normally
        not be modified during transport, but it may be lost (for instance
        if the network cable is cut...) *)

  val events : t -> Bencode.t Signal.t
    (** Events: received messages, sent by the remote side *)

  val local : int -> t option Lwt.t

  val by_host : string -> int -> t option Lwt.t

  val by_name : string -> int -> t option Lwt.t
    (** use DNS to resolve the given address, then call {!by_host} *)

  val of_sock : Lwt_unix.file_descr -> Address.t -> t
    (** Build a connection from a given filedescriptor *)
end

(** {2 Connections from remote machines} *)

module Server : sig
  type receive_ev

  val reply : receive_ev -> Bencode.t -> unit
    (** Reply to the event by sending a message *)

  val msg : receive_ev -> Bencode.t
    (** Message contained in the receive_ev *)

  val addr : receive_ev -> Address.t
    (** Address of the sender *)

  type event =
    | Receive of receive_ev
    | Stop  (* stopped receiving messages*)

  type t
    (** A server interface, that listens on some port on the network *)

  val events : t -> event Signal.t
    (** Signal transmitting events that occur on the server, when the
        server stops or a message is received *)

  val create : ?retry:int -> ?port:int -> ?log:bool -> unit -> t option
    (** Create a new network node on the given port, if provided
        (a random port otherwise). May return None if it is impossible
        to create a socket on the given port.
        @param retry number of times a random port is tried if the previous
          one is occypied (default 3)
        @param log: if true, then print events on stdout, if false or
          unspecified does nothing *)

  val port : t -> int
    (** Port used by the network server *)

  val stop : t -> unit
    (** Stop this server. It cannot be used anymore afterward. *)

  val wait : t -> unit Lwt.t
    (** Wait for the server to stop *)
end
