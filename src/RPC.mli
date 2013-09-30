
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

type address = Net_tcp.Address.t

type t
  (** A RPC system *)

and result =
  | NoReply
  | Reply of Bencode.t list
  | Error of string

and method_ = Bencode.t list -> result Lwt.t
  (** A method can return a result, given a list of arguments *)

val create : ?frequency:float -> Net_tcp.t -> t
  (** Create an instance of the RPC system, which can send and receive
      remote function calls using the [Net_tcp.t] instance.
      [frequency] is the frequency, in seconds, at which the
      RPC system checks whether some replies timed out. *)

val stop : t -> unit
  (** Disable all threads and active processes *)

(** {3 RPC interface} *)

val register : t -> string -> method_ -> unit
  (** [register rpc name f] registers [f] under the given [name]. Calls
      to [name] will be handled to [f].
      @raise Failure when the name is already taken. *)

val call : ?timeout:float -> t ->
           address -> string -> Bencode.t list -> result Lwt.t
  (** Call a remote method, with given name and arguments, and get
      a future reply *)

val call_ignore : t -> address -> string -> Bencode.t list -> unit
  (** Call a remote method, without expecting result. Errors
      will not be reported. *)
