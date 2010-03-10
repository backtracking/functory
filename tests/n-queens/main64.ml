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

(* Number of solutions to the n-queens puzzle using Map/Reduce *)

open Format

(* open Mapreduce.Sequential *)

(* open Mapreduce.Cores *)
(* let () = set_number_of_cores 2 *)

let () = Mapreduce.Control.set_debug true
open Mapreduce
let () = Network.declare_workers ~n:12 "moloch"
let () = Network.declare_workers ~n:1 "localhost"
let () = Network.declare_workers ~n:4 "orcus"

let rec t a b c count =
  if a > 0 then
    let rec loop e count =
      if e > 0 then
	let d = e land (-e) in
	loop (e - d) (t (a-d) ((b+d)*2) ((c+d)/2) count)
      else
	count
    in
    loop (a land lnot b land lnot c) count
  else
    Int64.succ count

let problems = ref []

let rec split depth a b c =
  if depth = 0 then
    problems := (a,b,c) :: !problems
  else
    let rec loop e =
      if e > 0 then begin
	let d = e land (-e) in
	split (depth-1) (a-d) ((b+d)*2) ((c+d)/2);
	loop (e - d) 
      end
    in
    loop (a land lnot b land lnot c)

let compute (a,b,c) = t a b c 0L

let n_queens q d = 
  let all = lnot ((lnot 0) lsl q) in
  split d all 0 0;
  printf "%d sub-problems@." (List.length !problems);
  Network.Same.map_local_fold ~map:compute ~fold:Int64.add 0L !problems

let test_n_queens q d = 
  printf "computing n-queens(%d) with depth %d...@." q d;
  let r = n_queens q d in
  printf "done (answer = %Ld)@." r

let () = test_n_queens (int_of_string Sys.argv.(1)) (int_of_string Sys.argv.(2))

(*
Local Variables: 
compile-command: "make -C ../.. install-queens"
End: 
*)


(* benchmark results on moloch

   N   implem.    real time 
  -------------------------
   16    simple   15.92s
        4 cores    4.15s
        8 cores    2.14s
       16 cores    2.17s

   17    simple 1m51.00s
        4 cores   31.70s
        8 cores   17.83s
       16 cores   16.90s
       17 cores   15.20s
*)

