
(* This file is free software, part of bencode_rpc. See file "license" for more details. *)

(** {1 Data Encoding} *)

type 'a or_error = ('a, string) Result.result

val (>>=) : 'a or_error -> ('a -> 'b or_error) -> 'b or_error
val (>|=) : 'a or_error -> ('a -> 'b) -> 'b or_error
val map_m : ('a -> 'b or_error) -> 'a list -> 'b list or_error

type 'a t = {
  encode: 'a -> Bencode.t;
  decode: Bencode.t -> 'a or_error;
}

val encode : 'a t -> 'a -> Bencode.t
val decode : 'a t -> Bencode.t -> 'a or_error

exception Decode_fail of string * Bencode.t

val decode_exn : 'a t -> Bencode.t -> 'a
(** Unsafe version of {!decode}
    @raise Decode_fail on decoding failure *)

val unit : unit t
val string : string t
val int : int t
val float : float t
val bool : bool t

val marshal : unit -> 'a t

val option : 'a t -> 'a option t
val pair : 'a t -> 'b t -> ('a * 'b) t
val triple : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t
val list : 'a t -> 'a list t
val array : 'a t -> 'a array t



