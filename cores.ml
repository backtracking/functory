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
open Unix

type 'a job = {
  worker : int;
  pid : int;
  file : file_descr; (* the pipe where to read the result from *)
  task : 'a; 
}

let create_worker w (f : 'a -> 'b) (x : 'a) : 'a job =
  let fin, fout = pipe () in
  match fork () with
    | 0 -> (* child *)
	close fin;
	let r = f x in
	let c = out_channel_of_descr fout in
	output_value c r;
	exit 0
    | pid -> (* parent *)
	close fout;
	{ worker = w;
	  pid = pid;
	  file = fin;
	  task = x }

module Make(P : sig val ncores : int end) = struct

  let rec listij acc i j = if i > j then acc else listij (j :: acc) i (j-1)
  let workers = listij [] 1 P.ncores

  (* master *)
  (****
  let map (f : 'a -> 'b) (l : 'a list) : 'b list = 
    let args = Array.of_list l in
    let tasks = listij [] 0 (Array.length args - 1) in
    let results = Hashtbl.create 17 in (* index -> 'b *)
    let jobs = Hashtbl.create 17 in (* PID -> job *)
    let module M = Master.Make
      (struct
	 type worker = int (* 1 .. ncores *)
	 let print_worker = Format.pp_print_int
	 type task = int (* index in l *)
	 let create_job w i =
	   let j = create_worker w (fun i -> f args.(i)) i in
	   Hashtbl.add jobs j.pid j
	 let wait () = match Unix.wait () with
	   | p, WEXITED e -> 
	       let j = Hashtbl.find jobs p in (* TODO: make it more robust *)
	       let c = in_channel_of_descr j.file in
	       let r : 'b = input_value c in
	       close_in c;
	       Hashtbl.add results j.task r;
	       j.worker, []
	   | p, _ ->
	       eprintf "master: PID %d killed or stopped!@." p;
	       exit 1
       end)
    in
    M.run workers tasks;
    List.map (fun i -> Hashtbl.find results i) tasks
  ***)

  let master ~(f : 'a -> 'b) ~(handle : 'a job -> 'b -> 'a list) tasks =
    let jobs = Hashtbl.create 17 in (* PID -> job *)
    Master.run
      ~create_job:(fun w t ->
		     let j = create_worker w f t in
		     Hashtbl.add jobs j.pid j)
      ~wait:(fun () -> match Unix.wait () with
	      | p, WEXITED e -> 
		  let j = Hashtbl.find jobs p in (* TODO: make it more robust *)
		  let c = in_channel_of_descr j.file in
		  let r : 'b = input_value c in
		  close_in c;
		  let l = handle j r in j.worker, l
	      | p, _ ->
		  eprintf "master: PID %d killed or stopped!@." p;
		  exit 1)
      workers tasks

  let map f l =
    let tasks = let i = ref 0 in List.map (fun x -> incr i; !i,x) l in
    let results = Hashtbl.create 17 in (* index -> 'b *)
    master 
      ~f:(fun (_,x) -> f x)
      ~handle:(fun {task=i,_} r -> Hashtbl.add results i r; [])
      tasks;
    List.map (fun (i,_) -> Hashtbl.find results i) tasks

  let fold ~(map : 'a -> 'b list) ~(reduce : 'c -> 'b -> 'c) acc l =
    let acc = ref acc in
    master 
      ~f:map
      ~handle:(fun _ r -> acc := List.fold_left reduce !acc r; [])
      l;
    !acc

  type ('a, 'b) map_reduce =
    | Map of 'a
    | Reduce of 'b

  let map_reduce ~(map : 'a -> 'b) ~(reduce : 'c -> 'b -> 'c) acc l =
    let acc = ref (Some acc) in
    let pending = Stack.create () in
    let do_reduce () = assert false (* TODO *) in
    master 
      ~f:(function
	    | Map x -> map x
	    | Reduce x -> begin match !acc with
		| Some v -> acc := None; reduce v x
		| None -> assert false
	      end)
      ~handle:(fun j r -> match j.task with
		 | Map _ -> begin match !acc with
		     | None -> Stack.push r pending; do_reduce ()
		     | Some _ -> [Reduce r]
		   end
		 | Reduce _ -> begin match !acc with
		     | None -> acc := Some r; do_reduce ()
		     | Some _ -> assert false
		   end)
      (List.map (fun x -> Map x) l);
    !acc

(***
  let map (f : 'a -> 'b) (l : 'a list) : 'b list = 
    let tasks = let i = ref 0 in List.map (fun x -> incr i; !i,x) l in
    let results = Hashtbl.create 17 in (* index -> 'b *)
    let jobs = Hashtbl.create 17 in (* PID -> job *)
    Master.run
      ~create_job:(fun w i ->
		     let j = create_worker w (fun (_,x) -> f x) i in
		     Hashtbl.add jobs j.pid j)
      ~wait:(fun () -> match Unix.wait () with
	      | p, WEXITED e -> 
		  let j = Hashtbl.find jobs p in (* TODO: make it more robust *)
		  let c = in_channel_of_descr j.file in
		  let r : 'b = input_value c in
		  close_in c;
		  Hashtbl.add results (fst j.task) r;
		  j.worker, []
	      | p, _ ->
		  eprintf "master: PID %d killed or stopped!@." p;
		  exit 1)
      workers tasks;
    List.map (fun (i,_) -> Hashtbl.find results i) tasks
***)
    
(****

    let tasks = let i = ref 0 in List.map (fun x -> incr i; (!i, x)) l in
    let todo = Stack.create () in
    List.iter (fun x -> Stack.push x todo) tasks;
    let towait = ref (Stack.length todo) in
    let idle = ref P.ncores in
    let results = Hashtbl.create 17 in (* task id -> result *)
    let workers = Hashtbl.create 17 in (* pid -> worker *)
    while not (Stack.is_empty todo) || !towait > 0 do
      (* if possible, start new workers *)
      while !idle > 0 && not (Stack.is_empty todo) do
	let t = Stack.pop todo in
	let w = create_worker (fun (_,x) -> f x) t in
	eprintf "master: started worker %d@." w.pid;
	Hashtbl.add workers w.pid w;
	decr idle
      done;
      (* otherwise, wait for results *)
      match wait () with
	| p, WEXITED e ->
	    eprintf "master: got result from PID %d@." p;
	    let w = Hashtbl.find workers p in (* TODO: make it more robust *)
	    let c = in_channel_of_descr w.file in
	    let r : 'b = input_value c in
	    close_in c;
	    Hashtbl.add results (fst w.task) r;
	    incr idle;
	    decr towait
	| p, _ ->
	    eprintf "master: PID %d killed or stopped!@." p;
	    exit 1
    done;
    assert (Stack.is_empty todo && !towait = 0);
    List.map (function (i,_) -> Hashtbl.find results i) tasks
****)

end
