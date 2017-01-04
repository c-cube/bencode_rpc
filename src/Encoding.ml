
(* This file is free software, part of bencode_rpc. See file "license" for more details. *)

(** {1 Data Encoding} *)

open Result
module B = Bencode

let (>|=) x f = match x with
  | Ok y -> Ok (f y)
  | Error msg -> Error msg

let (>>=) x f = match x with
  | Ok y -> f y
  | Error msg -> Error msg

let map_m f l =
  let rec aux acc l = match l with
    | [] -> Ok (List.rev acc)
    | x :: tail ->
      f x >>= fun x -> aux (x::acc) tail
  in
  aux [] l

type 'a or_error = ('a, string) Result.result

type 'a t = {
  encode: 'a -> Bencode.t;
  decode: Bencode.t -> 'a or_error;
}

let mk_ encode decode : _ t = {encode; decode}

let fail_expected what b =
  Error (Printf.sprintf "expected %s, got `%s`" what (B.encode_to_string b))

let as_string = function
  | B.String s -> Ok s
  | b -> fail_expected "string" b

let as_int = function
  | B.Integer i -> Ok i
  | b -> fail_expected "int" b

let as_float s =
  try Ok (float_of_string s)
  with Failure _ -> Error "invalid float"
let as_bool s =
  try Ok (bool_of_string s)
  with Failure msg -> Error "invalid bool"

let bimap f1 f2 (e:_ t): _ t =
  mk_ (fun x -> e.encode (f1 x)) (fun s -> e.decode s >>= f2)

let string = mk_ (fun s -> B.String s) as_string
let int = mk_ (fun x -> B.Integer x) as_int
let float = bimap string_of_float as_float string
let bool = bimap string_of_bool as_bool string

let marshal () =
  let encode x =
    B.List [
      B.String "marshal";
      B.String (Marshal.to_string x [Marshal.No_sharing; Marshal.Compat_32]);
    ]
  and decode = function
    | B.List [B.String "marshal"; B.String x] ->
      Ok (Marshal.from_string x (String.length x))
    | b -> fail_expected "marshal" b
  in
  mk_ encode decode

let option e =
  mk_
    (function
      | Some x -> B.List [e.encode x]
      | None -> B.List [])
    (function
      | B.List [y] -> (e.decode y >|= fun x->Some x)
      | B.List [] -> Ok None
      | b -> fail_expected "option" b)

let pair a b =
  mk_
    (fun (x,y) -> B.List [a.encode x; b.encode y])
    (function
      | B.List [x;y] -> (a.decode x >>= fun x -> b.decode y >|= fun y -> x,y)
      | b -> fail_expected "pair" b)

let triple a b c =
  mk_
    (fun (x,y,z) -> B.List [a.encode x; b.encode y; c.encode z])
    (function
      | B.List [x;y;z] ->
        (a.decode x >>= fun x -> b.decode y >>= fun y -> c.decode z >|= fun z -> x,y,z)
      | b -> fail_expected "triple" b)

let list e =
  mk_ (fun l -> B.List (List.map e.encode l))
    (function
      | B.List l -> map_m e.decode l
      | b -> fail_expected "list" b)

let array e = bimap Array.to_list (fun l->Ok (Array.of_list l)) (list e)



