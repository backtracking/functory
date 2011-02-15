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

(* Number of solutions to the n-queens puzzle using Map/Reduce *)

(* open Functory.Sequential *)

(* open Functory.Cores *)
(* let () = set_number_of_cores 2 *)

let () = Functory.Control.set_debug true
open Functory.Network
(* let () = Network.declare_workers ~n:2 "129.175.4.107" *)
(* let () = Network.declare_workers ~n:12 "moloch" *)
let () = declare_workers ~n:1 "localhost"
(* let () = Network.declare_workers ~n:4 "orcus" *)
open Same

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

let n_queens q = 
  let l = tabulate 0 (q-1) (fun i -> 1 lsl i) in
  let all = lnot ((lnot 0) lsl q) in
  let rl = map (fun c -> t (all-c) (c*2) (c/2) 0) l in (* <- map here *)
  List.fold_left (+) 0 rl	                 (* <- reduce locally *)

let test_n_queens q = 
  Format.printf "computing n-queens(%d)...@?" q;
  let r = n_queens q in
  Format.printf "done (answer = %d)@." r

let () = test_n_queens (int_of_string Sys.argv.(1))

(*
Local Variables: 
compile-command: "make -C ../.. tests/n-queens/a.out"
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

