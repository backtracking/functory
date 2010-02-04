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

open Control
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

let ncores = ref 1
let set_number_of_cores n = ncores := n

let rec listij acc i j = if i > j then acc else listij (j :: acc) i (j-1)
let workers () = listij [] 1 !ncores

(* the generic master *)
let master ~(f : 'a -> 'b) ~(handle : 'a job -> 'b -> 'a list) tasks =
  let jobs = Hashtbl.create 17 in (* PID -> job *)
  Master.run
    ~create_job:(fun w t ->
		   let j = create_worker w f t in
		   dprintf "master: started worker %d (PID %d)@." w j.pid;
		   Hashtbl.add jobs j.pid j)
    ~wait:(fun () -> match Unix.wait () with
	     | p, WEXITED e -> 
		 let j = Hashtbl.find jobs p in
		 dprintf "master: got result from worker %d@." j.worker;
		 let c = in_channel_of_descr j.file in
		 let r : 'b = input_value c in
		 close_in c;
		 let l = handle j r in j.worker, l
	     | p, _ ->
		 Format.eprintf "master: ** PID %d killed or stopped! **@." p;
		 exit 1)
    (workers ()) tasks


(* and its instances *)

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

