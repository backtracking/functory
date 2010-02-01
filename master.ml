
(** The master implementation *)

(** The master context is a notion of workers, tasks and jobs *)
module type CONTEXT = sig
  module Worker : sig 
    type t 
    val hash : t -> int 
    val equal : t -> t -> bool 
  end
  type 'a task

  type 'a job
  val create_job : Worker.t -> 'a task -> 'a job
    (** We create a job by assigning a given task to a given worker.
        It is guaranted that each worker will be assigned only one task at
        a time. *)

  val wait : unit -> Worker.t list

  val extract : 'a job -> 'a task list
end

module Make(C : CONTEXT) : sig 

  val run : C.Worker.t list -> 'a C.task list -> unit

end = struct

end


