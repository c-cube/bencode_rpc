
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

(** {6 RPC} *)

(** This provides a lightweight RPC mechanism on top of a network
    implementation and B-encoded messages. *)

type address = NetTcp.Address.t

type 'a result =
  | NoReply
  | Reply of 'a
  | Error of string

type t

(** {2 Basics} *)

val connection : t -> NetTcp.Connection.t
  (** Underlying connection *)

val address : t -> address
  (** Remote address *)

val is_alive : t -> bool
  (** Is the proxy alive? *)

val close : t -> unit
  (** Close the connection and make the proxy unusable. *)

val call : ?timeout:float -> t -> string -> Bencode.t -> Bencode.t result Lwt.t
  (** Call a remote method, with given name and arguments, and get
      a future reply
      @param timeout number of seconds without answer before we
        decide that no answer will come. After this delay, the result
        will be {!Error}. By default, 20 seconds. *)

val call_ignore : t -> string -> Bencode.t -> unit
  (** Call a remote method, without expecting result. Errors
      will not be reported. *)

(** {2 Typed method invocation} *)

module Typed : sig
  type ('a,'b) method_
    (** A remote method, with parameter type 'a and return type 'b, so
        basically a RPC equivalent of 'a -> 'b. *)

  val create : name:string ->
              encode:('a -> Bencode.t) ->
              decode:(Bencode.t -> 'b option) ->
              ('a,'b) method_
    (** Create a representation of a remote method, where parameters are
        serialized with [encode] and result deserialized via [decode] *)

  val call : ?timeout:float -> t -> ('a,'b) method_ -> 'a -> 'b result Lwt.t
    (** Call a remote method, encoding and decoding parameters and result. *)

  val call_ignore : t -> ('a,_) method_ -> 'a -> unit
    (** Call a remote method, ignoring the result *)
end

(** {2 Creation of proxy} *)

(** A proxy is parametrized by a [period]. This period, measured in seconds,
    is the amount of time between two successive checks of query
    expiration. Every [period] seconds, the system removes method calls
    that have timeouted, and close them (by sending {!Error}) *)

val of_conn : ?period:float -> NetTcp.Connection.t -> t
  (** Create a proxy from an existing connection
      @return a proxy that acts as a "remote" object on which methods can be
        called, or None in case of failure *)

val connect : ?period:float -> address -> t option Lwt.t
  (** Connect to a remote node by address. *)

val by_host : ?period:float -> string -> int -> t option Lwt.t

val local : ?period:float -> int -> t option Lwt.t

val by_name : ?period:float -> string -> int -> t option Lwt.t
  (** DNS lookup before connecting *)

val fmt : Format.formatter -> t -> unit
