
(* This file is free software, part of bencode_rpc. See file "license" for more details. *)

(** {1 Lwt-compatible blocking queue} *)

let (>>=) = Lwt.(>>=)

type 'a t = {
  queue : 'a Queue.t;
  cond : unit Lwt_condition.t;
}

let create () =
  { queue = Queue.create ();
    cond = Lwt_condition.create ();
  }

let is_empty q =
  Queue.is_empty q.queue

let push q x =
  Queue.push x q.queue;
  Lwt_condition.signal q.cond ()

let rec pop q =
  if Queue.is_empty q.queue
  then
    Lwt_condition.wait q.cond >>= fun () ->
    let x = Queue.pop q.queue in
    Lwt.return x
  else
    Lwt.return (Queue.pop q.queue)

let clear q =
  Queue.clear q.queue
