
(** The generic master implementation *)

val run :
  create_job:('worker -> 'task -> unit) ->
  wait:(unit -> 'worker * 'task list) ->
  'worker list ->
  'task list -> 
  unit

