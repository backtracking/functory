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

open Mapreduce.Cores
include Make(struct let ncores = 1 end)

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
    count+1

(* the list [f i; f (i+1); ...; f j] *)
let rec tabulate i j f = if i > j then [] else f i :: tabulate (i+1) j f

let () = 
  let q = int_of_string Sys.argv.(1) in
  let l = tabulate 0 (q-1) (fun i -> 1 lsl i) in
  let all = lnot ((lnot 0) lsl q) in
  let rl = map (fun c -> t (all-c) (c*2) (c/2) 0) l in (* <- map here *)
  let r = List.fold_left (+) 0 rl in	               (* <- reduce locally *)
  Format.printf "%d@." r

(*
Local Variables: 
compile-command: "make -C ../.. tests/n-queens/a.out"
End: 
*)


