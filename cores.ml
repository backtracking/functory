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

let long_computation () =
  sleep 3;
  42

let process (f : 'a -> 'b) (x : 'a) : 'b =
  let file = Filename.temp_file "result" "" in
  match fork () with
    | 0 -> (* child *)
	let r = f x in
	let c = open_out file in
	output_value c r;
	close_out c;
	exit 0
    | pid -> (* parent *)
	match wait () with
	  | p, WEXITED e ->
	      assert (p = pid);
	      printf "PID %d: exit code = %d@." p e;
	      let c = open_in file in
	      let r = input_value c in
	      close_in c;
	      r
	  | p, _ ->
	      printf "PID %d: killed or stopped!@." p;
	      failwith "process"

(* TODO *)
let map = List.map
