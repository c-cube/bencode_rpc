
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

(** {1 Simple broadcast}

This module implements a simple networking protocol based on naive
broadcasting. Nodes simply broadcast the messages to their
neighbors, and keep a table of recently received messages to
eliminate duplicates.
*)

type address = NetTcp.Address.t

type t
  (** Broadcasting algorithm state. A value of this type keeps a list
      of neighbors, a RPC server with some methods, and a cache of recently
      received messages.
      
      Messages are compared as raw {! Bencode.t} values. Sending twice
      the same message within a short amount of time will result
      in only one broadcast. If you need sending twice the same message,
      consider adding some nonce to it (timestamp, random value...) *)

val create : ?cache_timeout:float -> RPCServer.t -> t
  (** Create a new broadcasting device.
      @param cache_timeout the number of seconds before a received message
        is forgotten. Messages are kept in cache to prevent loops. *)

val broadcast : t -> Bencode.t -> unit
  (** Broadcast a message to everyone in the network *)

val connect : t -> address -> bool Lwt.t
  (** Connect to another node. Returns true iff it succeeds. *)

val neighbors : t -> address list
  (** Current list of neighbors *)

type event =
  | Receive of Bencode.t
  | NewNeighbor of address

val events : t -> event Signal.t
  (** messages broadcasted by others *)

val recv : t -> event Lwt.t
  (** wait for the next event that occurs on the network *)
