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
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

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

  type event =
    | Receive of receive_ev
    | Stop  (* stopped receiving messages*)

  type t
    (** A server interface, that listens on some port on the network *)

  val events : t -> event Signal.t
    (** Signal transmitting events that occur on the server, when the
        server stops or a message is received *)

  val create : ?retry:int -> ?port:int ->
              unit -> t option
    (** Create a new network node on the given port, if provided
        (a random port otherwise). May return None if it is impossible
        to create a socket on the given port.
        @param retry number of times a random port is tried if the previous
          one is occypied (default 3) *)

  val port : t -> int
    (** Port used by the network server *)

  val stop : t -> unit
    (** Stop this server. It cannot be used anymore afterward. *)

  val wait : t -> unit Lwt.t
    (** Wait for the server to stop *)
end
