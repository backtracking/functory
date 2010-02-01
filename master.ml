
(** The generic master implementation *)

(** The master requires a notion of workers, tasks and jobs *)

module type WORKER = sig 
  type t 
  val hash : t -> int 
  val equal : t -> t -> bool 
end

module type CONTEXT = sig
  type worker

  type 'a task

  type 'a job
  val create_job : worker -> 'a task -> 'a job
    (** We create a job by assigning a given task to a given worker.
        It is guaranted that each worker will be assigned only one task at
        a time. *)

  val wait : unit -> worker list
    (** When the master is idle, it waits for worker completions using this
	function. This function must return only workers for which a task was
	assigned and is now completed. *)

  val extract : 'a job -> 'a task list
    (** Whenever a job is completed, it is passed to this function, which may
	produce new tasks in return. *)
end

(** Given a context, we provide a master implementation *)
module Make(W : WORKER)(C : CONTEXT with type worker = W.t) : sig 

  val run : C.worker list -> 'a C.task list -> unit
    (** Runs the master with the given list of workers and tasks.
	It returns only when there are not more pending tasks. *)

end = struct

  let run wl tl = 
    assert false (* TODO *)

end


