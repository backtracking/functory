(**************************************************************************)
(*                                                                        *)
(*  Functory: a distributed computing library for OCaml                   *)
(*  Copyright (C) 2010- Jean-Christophe Filliatre and Kalyan Krishnamani  *)
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
open Control
open Unix

(* main loop: assigns tasks to workers, until no more task *)
let run
    ~(create_job : 'worker -> 'task -> unit)
    ~(wait : unit -> 'worker * 'task list)
    (workers : 'worker list)
    (tasks : 'task list)
    =
  let todo = Queue.create () in
  List.iter (fun t -> Queue.push t todo) tasks;
  let towait = ref 0 in
  let idle = Queue.create () in
  List.iter (fun w -> Queue.push w idle) workers;
  while not (Queue.is_empty todo) || !towait > 0 do
    (* if possible, start new workers *)
    while not (Queue.is_empty idle) && not (Queue.is_empty todo) do
      let t = Queue.pop todo in
      let w = Queue.pop idle in
      create_job w t;
      incr towait
    done;
    assert (!towait > 0);
    (* otherwise, wait for results *)
    let w, tl = wait () in
    decr towait;
    Queue.push w idle;
    List.iter (fun t -> Queue.push t todo) tl
  done;
  assert (Queue.is_empty todo && !towait = 0)

let ncores = ref 1
let set_number_of_cores n = ncores := n

let rec listij acc i j = if i > j then acc else listij (j :: acc) i (j-1)
let workers () = listij [] 1 !ncores

(*** using local files ***************************************************)

type 'a job = {
  worker : int;
  pid : int;
  file : string;
  task : 'a;
}

let create_worker w (f : 'a -> 'b) (t : 'a * 'c) : ('a * 'c) job =
  let file = Filename.temp_file "mapfold" "output" in
  match fork () with
    | 0 -> (* child *)
	let r = f (fst t) in
	let c = open_out file in
	output_value c r;
	close_out c;
	exit 0
    | pid -> (* parent *)
	{ worker = w;
	  pid = pid;
	  file = file;
	  task = t }

exception Worker of int

let compute
    ~(worker : 'a -> 'b) ~(master : ('a * 'c) -> 'b -> ('a * 'c) list) tasks =
  let jobs = Hashtbl.create 17 in (* PID -> job *)
  let rec wait () =
    try
      Hashtbl.iter (fun pid _ -> match Unix.waitpid [] pid with
      | _, WEXITED e ->
        dprintf "master: got result from worker PID %d@." pid;
        raise (Worker pid)
      | _ ->
        Format.eprintf "master: ** PID %d killed or stopped! **@." pid)
        jobs;
      wait ()
    with Worker pid ->
      let j = Hashtbl.find jobs pid in
      Hashtbl.remove jobs pid;
      dprintf "master: got result from worker %d@." j.worker;
      let c = open_in j.file in
      let r : 'b = input_value c in
      close_in c;
      Sys.remove j.file;
      let l = master j.task r in
      j.worker, l
  in
  try
    run
      ~create_job:(fun w t ->
	let j = create_worker w worker t in
	dprintf "master: started worker %d (PID %d)@." w j.pid;
	Hashtbl.add jobs j.pid j)
      ~wait (workers ()) tasks
  with e ->
    Hashtbl.iter (fun p _ -> try Unix.kill p Sys.sigkill with _ -> ()) jobs;
    raise e

(* derived API *)

include Map_fold.Make(struct let compute = compute end)

