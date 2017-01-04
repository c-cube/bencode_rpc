
(* This file is free software, part of bencode_rpc. See file "license" for more details. *)

(** {1 Basic signal} *)

type 'a t
  (** Signal of type 'a *)

val create : unit -> 'a t
  (** New signal *)

val send : 'a t -> 'a -> unit
  (** Trigger the signal *)

val on : 'a t -> ('a -> bool) -> unit
  (** Register a handler to the signal; the handler returns [true]
      if it wants to continue being notified, [false] otherwise *)

val once : 'a t -> ('a -> 'b) -> unit
  (** Register a handler to be called only once *)

val propagate : 'a t -> 'a t -> unit
  (** [propagate a b] propagates all values of [a] into [b]. Cycles
      are not detected. *)
