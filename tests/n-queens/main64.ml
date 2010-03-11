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
compile-command: "unset LANG; make -C ../.. install-queens"
End: 
*)


(* benchmark results using 17 cores: moloch(12)/orcus(4)/balrog(1)

   d is the depth

   N  d  #tasks time 
  -------------------------
   15 1         0m 1.153s

   16 1         0m 2.049s
   16 2         0m 9.458s
   16 3         --- network >> computation

   17 1         0m11.022s
   17 2         0m17.291s
   17 3         --- network >> computation

   18 1         1m22.263s
   18 2         1m12.856s
   18 3         --- network >> computation

   19 1   19
   19 2  306    9m 9.724s
   19 3        10m11.596s

   20 2        71m30.663s
   20 3

*)
