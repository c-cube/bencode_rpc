
(* This file is free software, part of bencode_rpc. See file "license" for more details. *)

(** {1 Basic signal} *)

type 'a t = {
  mutable n : int;  (* how many handlers? *)
  mutable handlers : ('a -> bool) array;
} (** Signal of type 'a *)

let nop_handler x = true

let create () =
  let s = { 
    n = 0;
    handlers = Array.create 3 nop_handler;
  } in
  s

(* remove handler at index i *)
let remove s i =
  (if i < s.n - 1  (* erase handler with the last one *)
    then s.handlers.(i) <- s.handlers.(s.n - 1));
  s.handlers.(s.n - 1) <- nop_handler; (* free handler *)
  s.n <- s.n - 1;
  ()

let send s x =
  for i = 0 to s.n - 1 do
    while not (try s.handlers.(i) x with _ -> false) do
      remove s i  (* i-th handler is done, remove it *)
    done
  done

let on s f =
  (* resize handlers if needed *)
  (if s.n = Array.length s.handlers
    then begin
      let handlers = Array.create (s.n + 4) nop_handler in
      Array.blit s.handlers 0 handlers 0 s.n;
      s.handlers <- handlers
    end);
  s.handlers.(s.n) <- f;
  s.n <- s.n + 1

let once s f =
  on s (fun x -> ignore (f x); false)

let propagate a b =
  on a (fun x -> send b x; true)
