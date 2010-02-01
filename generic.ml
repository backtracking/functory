
module type WORKER = sig
  type worker
  type 'a job
  type 'a task
end

module Master(W : WORKER) : sig 

  val add_worker : W.worker -> unit

  val add_task : 'a W.task -> unit

end = struct

end


