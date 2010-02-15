(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) Jean-Christophe Filliatre and Kalyan Krishnamani        *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2.1, with the special exception on linking            *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

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

