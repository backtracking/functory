
(** The generic master implementation. *)

val run : create_job:('worker -> 'task -> unit) ->
          wait:(unit -> 'worker * 'task list) ->
          'worker list -> 'task list -> unit
  (** [run create_job wait wl tl] implements the main loop of the
      master.  It assigns tasks from list [tl] to workers from list
      [wl], until all tasks are completed.  To assign a task to a
      worker, it uses the function [create_job] passed as argument. To
      wait for completed task, it uses the function [wait] passed as
      argument. [wait] returns one worker which has completed its task
      (and may block as long as there is none), together with new tasks, 
      if any. *)

