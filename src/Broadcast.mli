

(* This file is free software, part of bencode_rpc. See file "license" for more details. *)

(** {1 Simple broadcast}

This module implements a simple networking protocol based on naive
broadcasting. Nodes simply broadcast the messages to their
neighbors, and keep a table of recently received messages to
eliminate duplicates.
*)

type address = Net_tcp.Address.t

type t
  (** Broadcasting algorithm state. A value of this type keeps a list
      of neighbors, a RPC server with some methods, and a cache of recently
      received messages.
      
      Messages are compared as raw {! Bencode.t} values. Sending twice
      the same message within a short amount of time will result
      in only one broadcast. If you need sending twice the same message,
      consider adding some nonce to it (timestamp, random value...) *)

val create : ?cache_timeout:float -> RPC_server.t -> t
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
