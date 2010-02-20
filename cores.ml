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
open Control
open Unix

(***
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
***)

type 'a job = {
  worker : int;
  pid : int;
  file : string;
  task : 'a; 
}
let create_worker w (f : 'a -> 'b) (x : 'a) : 'a job =
  let file = Filename.temp_file "mapfold" "output" in
  match fork () with
    | 0 -> (* child *)
	let r = f x in
	let c = open_out file in
	output_value c r;
	exit 0
    | pid -> (* parent *)
	{ worker = w;
	  pid = pid;
	  file = file;
	  task = x }


let ncores = ref 1
let set_number_of_cores n = ncores := n

let rec listij acc i j = if i > j then acc else listij (j :: acc) i (j-1)
let workers () = listij [] 1 !ncores

(* the generic master *)
let master ~(f : 'a -> 'b) ~(handle : 'a -> 'b -> 'a list) tasks =
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
          let l = handle j.task r in j.worker, l
        with Not_found -> 
          (* If the pid is unknown to us, it's probably a process created by one
             of the workers. In this case, simply continue to wait. *)
          wait () end
    | p, _ ->
        Format.eprintf "master: ** PID %d killed or stopped! **@." p;
        exit 1
  in
  Master.run
    ~create_job:(fun w t ->
		   let j = create_worker w f t in
		   dprintf "master: started worker %d (PID %d)@." w j.pid;
		   Hashtbl.add jobs j.pid j)
    ~wait (workers ()) tasks


(* and its instances *)

let map ~f l =
  let tasks = let i = ref 0 in List.map (fun x -> incr i; !i,x) l in
  let results = Hashtbl.create 17 in (* index -> 'b *)
  master 
    ~f:(fun (_,x) -> f x)
    ~handle:(fun (i,_) r -> Hashtbl.add results i r; [])
    tasks;
  List.map (fun (i,_) -> Hashtbl.find results i) tasks

let map_local_fold ~(map : 'a -> 'b) ~(fold : 'c -> 'b -> 'c) acc l =
  let acc = ref acc in
  master 
    ~f:map
    ~handle:(fun _ r -> acc := fold !acc r; [])
    l;
  !acc 

type ('a, 'b) map_fold =
  | Map of 'a
  | Fold of 'b

let map_remote_fold ~(map : 'a -> 'b) ~(fold : 'c -> 'b -> 'c) acc l =
  let acc = ref (Some acc) in
  let pending = Stack.create () in
  master 
    ~f:(function
	  | Map x -> Map (map x)
	  | Fold (v, x) -> Fold (fold v x))
    ~handle:(fun _ r -> match r with
	       | Map r -> begin match !acc with
		   | None -> Stack.push r pending; []
		   | Some v -> acc := None; [Fold (v, r)]
		 end
	       | Fold r -> 
		   assert (!acc = None);
		   if not (Stack.is_empty pending) then
		     [Fold (r, Stack.pop pending)]
		   else begin
		     acc := Some r;
		     []
		   end)
    (List.map (fun x -> Map x) l);
  (* we are done; the accumulator must exist *)
  match !acc with
    | Some r -> r
    | None -> assert false

let map_fold_ac ~(map : 'a -> 'b) ~(fold : 'b -> 'b -> 'b) acc l =
  let acc = ref (Some acc) in
  master 
    ~f:(function
	  | Map x -> map x
	  | Fold (v, x) -> fold v x)
    ~handle:(fun _ r -> match !acc with
	       | None -> acc := Some r; []
	       | Some v -> acc := None; [Fold (v, r)])
    (List.map (fun x -> Map x) l);
  (* we are done; the accumulator must exist *)
  match !acc with
    | Some r -> r
    | None -> assert false


let map_fold_a ~(map : 'a -> 'b) ~(fold : 'b -> 'b -> 'b) acc l =
  let tasks = let i = ref 0 in List.map (fun x -> incr i; !i, x) l in
  (* results maps i and j to (i,j,r) for each completed reduction
     of the interval i..j with result r *)
  let results = Hashtbl.create 17 in 
  let merge i j r = 
    assert (i <= j);
    if Hashtbl.mem results (i-1) then begin
      let l, h, x = Hashtbl.find results (i-1) in
      assert (l <= h && h = i-1);
      Hashtbl.remove results l; 
      Hashtbl.remove results h;
      [Fold (l, j, x, r)]
    end else if Hashtbl.mem results (j+1) then begin
      let l, h, x = Hashtbl.find results (j+1) in
      assert (l <= h && l = j+1);
      Hashtbl.remove results h; 
      Hashtbl.remove results l;
      [Fold (i, h, r, x)]
    end else begin
      Hashtbl.add results i (i,j,r);
      Hashtbl.add results j (i,j,r);
      []
    end
  in
  master 
    ~f:(function
	  | Map (_,x) -> map x
	  | Fold (_, _, x1, x2) -> fold x1 x2)
    ~handle:(fun x r -> match x with
	       | Map (i, _) -> merge i i r
	       | Fold (i, j, _, _) -> merge i j r)
    (List.map (fun x -> Map x) tasks);
  (* we are done; results must contain 2 mappings only, for 1 and n *)
  try let _,_,r = Hashtbl.find results 1 in r with Not_found -> acc

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


