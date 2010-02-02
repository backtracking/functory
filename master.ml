
open Format

(***
module type CONTEXT = sig
  type worker

  type task

  val create_job : worker -> task -> unit
    (** We create a job by assigning a given task to a given worker.
        It is guaranted that each worker will be assigned only one task at
        a time. *)

  val wait : unit -> worker * task list
    (** When the master is idle, it waits for worker completions using this
	function. This function must return only workers for which a task was
	assigned and is now completed. *)

  val print_worker : Format.formatter -> worker -> unit

end

(** Given a context, we provide a master implementation *)
module Make(C : CONTEXT) : sig 

  val run : C.worker list -> C.task list -> unit
    (** Runs the master with the given list of workers and tasks.
	It returns only when there are no more pending tasks. *)

end = struct

  let run workers tasks = 
    let todo = Stack.create () in
    List.iter (fun t -> Stack.push t todo) tasks;
    let towait = ref 0 in
    let idle = Stack.create () in
    List.iter (fun w -> Stack.push w idle) workers;
    while not (Stack.is_empty todo) || !towait > 0 do
      (* if possible, start new workers *)
      while not (Stack.is_empty idle) && not (Stack.is_empty todo) do
	let t = Stack.pop todo in
	let w = Stack.pop idle in
	eprintf "master: started worker %a@." C.print_worker w;
	C.create_job w t;
	incr towait
      done;
      assert (!towait > 0);
      (* otherwise, wait for results *)
      let w, tl = C.wait () in
      eprintf "master: got result from worker %a@." C.print_worker w;
      decr towait;
      Stack.push w idle;
      List.iter (fun t -> Stack.push t todo) tl
    done;
    assert (Stack.is_empty todo && !towait = 0)

end
***)

let run 
    ~(create_job : 'worker -> 'task -> unit) 
    ~(wait : unit -> 'worker * 'task list) 
    (workers : 'worker list)
    (tasks : 'task list)
    = 
  let todo = Stack.create () in
  List.iter (fun t -> Stack.push t todo) tasks;
  let towait = ref 0 in
  let idle = Stack.create () in
  List.iter (fun w -> Stack.push w idle) workers;
  while not (Stack.is_empty todo) || !towait > 0 do
    (* if possible, start new workers *)
    while not (Stack.is_empty idle) && not (Stack.is_empty todo) do
      let t = Stack.pop todo in
      let w = Stack.pop idle in
      (* eprintf "master: started worker %a@." C.print_worker w; *)
      create_job w t;
      incr towait
    done;
    assert (!towait > 0);
    (* otherwise, wait for results *)
    let w, tl = wait () in
    (* eprintf "master: got result from worker %a@." C.print_worker w; *)
    decr towait;
    Stack.push w idle;
    List.iter (fun t -> Stack.push t todo) tl
  done;
  assert (Stack.is_empty todo && !towait = 0)

