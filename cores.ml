(**************************************************************************)
(*                                                                        *)
(*  Functory: a distributed computing library for Ocaml                   *)
(*  Copyright (C) 2010 Jean-Christophe Filliatre and Kalyan Krishnamani   *)
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
	exit 0
    | pid -> (* parent *)
	{ worker = w;
	  pid = pid;
	  file = file;
	  task = t }


let ncores = ref 1
let set_number_of_cores n = ncores := n

let rec listij acc i j = if i > j then acc else listij (j :: acc) i (j-1)
let workers () = listij [] 1 !ncores

(* main loop: assigns tasks to workers, until no more task *)
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

let compute 
    ~(worker : 'a -> 'b) ~(master : ('a * 'c) -> 'b -> ('a * 'c) list) tasks =
  let jobs = Hashtbl.create 17 in (* PID -> job *)
  let rec wait () = 
    match Unix.wait () with
    | p, WEXITED e -> 
        dprintf "master: got result from worker PID %d@." p;
        begin try
          let j = Hashtbl.find jobs p in
          dprintf "master: got result from worker %d@." j.worker;
          let c = open_in (*in_channel_of_descr *) j.file in
          let r : 'b = input_value c in
          close_in c;
          Sys.remove j.file;
          let l = master j.task r in j.worker, l
        with Not_found -> 
          (* If the pid is unknown to us, it's probably a process created
	     by one of the workers. In this case, simply continue to wait. *)
          wait () 
	end
    | p, _ ->
        Format.eprintf "master: ** PID %d killed or stopped! **@." p;
        exit 1
  in
  run
    ~create_job:(fun w t ->
		   let j = create_worker w worker t in
		   dprintf "master: started worker %d (PID %d)@." w j.pid;
		   Hashtbl.add jobs j.pid j)
    ~wait (workers ()) tasks


include Map_fold.Make(struct let compute = compute end)

(*******

type ('a, 'b) map_reduce =
  | Map of 'a
  | Reduce of 'b

let map_reduce ~map ~reduce l =
  let results = Hashtbl.create 17 in 
  let to_reduce = Hashtbl.create 17 in
  let add k2 v2l =
    try 
      let l = Hashtbl.find results k2 in
      Hashtbl.replace results k2 (v2l :: l);
      Hashtbl.replace to_reduce k2 ()
    with Not_found ->
      Hashtbl.add results k2 [v2l]
  in
  let reduce_tasks () = 
    let tl = 
      Hashtbl.fold 
	(fun k2 _ acc -> (Reduce (k2, Hashtbl.find results k2)) :: acc) 
	to_reduce []
    in
    Hashtbl.iter (fun x _ -> Hashtbl.remove results x) to_reduce;
    Hashtbl.clear to_reduce;
    tl
  in
  master
    ~f:(function
	  | Map v1 -> Map (map v1)
	  | Reduce (k2, v2l) -> Reduce (reduce k2 v2l))
    ~handle:(fun x r -> 
	       match x, r with
		 | Map _, Map r -> 
		     List.iter (fun (k2, v2) -> add k2 [v2]) r; reduce_tasks ()
		 | Reduce (k2, _), Reduce r -> 
		     add k2 r; reduce_tasks ()
		 | _ ->
		     assert false)
    (List.map (fun x -> Map x) l);
  Hashtbl.fold 
    (fun k2 v2l res -> match v2l with
       | [v2l] -> (k2, v2l) :: res
       | _ -> assert false) 
    results []


**********)
