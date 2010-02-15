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

open Format

(* note: you cannot write it as a functor (because of type variables that
   can't be generalized) so it is provided as a higher-order function

   less readable but equivalent
*)

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
      create_job w t;
      incr towait
    done;
    assert (!towait > 0);
    (* otherwise, wait for results *)
    let w, tl = wait () in
    decr towait;
    Stack.push w idle;
    List.iter (fun t -> Stack.push t todo) tl
  done;
  assert (Stack.is_empty todo && !towait = 0)

