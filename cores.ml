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

type 'a worker = {
  pid : int;
  file : file_descr; (* the pipe where to read the result from *)
  task : 'a;
}

let create_worker (f : 'a -> 'b) (x : 'a) : 'a worker =
  let fin, fout = pipe () in
  match fork () with
    | 0 -> (* child *)
	let r = f x in
	let c = out_channel_of_descr fout in
	output_value c r;
	close_out c;
	exit 0
    | pid -> (* parent *)
	{ pid = pid;
	  file = fin;
	  task = x }

module Make(P : sig val ncores : int end) = struct

  let map (f : 'a -> 'b) (l : 'a list) : 'b list = 
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

end
