
(*
copyright (c) 2013, simon cruanes
all rights reserved.

redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.  redistributions in binary
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

(** {1 RPC server} *)

type address = Net_tcp.Address.t

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

val of_server : Net_tcp.Server.t -> t
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

(** {3 Helpers to build methods} *)

val reply : Bencode.t -> result Lwt.t

val no_reply : result Lwt.t

val error : String.t -> result Lwt.t
