
(* This file is free software, part of bencode_rpc. See file "license" for more details. *)

(** {1 Lwt-compatible blocking queue} *)

type 'a t

val create : unit -> 'a t

val is_empty : 'a t -> bool

val push : 'a t -> 'a -> unit

val pop : 'a t -> 'a Lwt.t

val clear : 'a t -> unit
