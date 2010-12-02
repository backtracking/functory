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

(* sorting a file using map/reduce *)

open Format

(* let () = Functory.Control.set_debug true *)

(* open Functory.Sequential *)

open Functory.Cores
let () = Functory.Cores.set_number_of_cores 2

(* open Functory.Network *)
(* let () = declare_workers ~n:4 "moloch" *)
(* let () = declare_workers ~n:2 "orcus" *)

let file = Sys.argv.(1)
let s = int_of_string Sys.argv.(2) (* chunk size *)

let () = 
  printf "reading file '%s'... @?" file

let list =
  let c = open_in file in
  let rec read acc =
    let l = try Some (input_line c) with End_of_file -> None in
    match l with None -> close_in c; List.rev acc | Some l -> read (l :: acc)
  in
  read []

let n = List.length list
let () = printf "done@.%d lines@." n

(* splitting list into chunks *)

let rec create_chunk acc i = function
  | [] -> acc, []
  | l when i = 0 -> acc, l
  | x :: l -> create_chunk (x :: acc) (i - 1) l

let rec split acc l =
  if l = [] then acc 
  else let c, l = create_chunk [] s l in split (c :: acc) l

let chunks = split [] list

(* merging lists *)

let cmp = String.compare

(* let merge = List.merge cmp *)

let merge l1 l2 =
  (* printf "merging %d %d...@." (List.length l1) (List.length l2); *)
  let rec merge acc = function
    | [], l | l, [] -> 
	List.rev_append acc l
    | x1 :: r1, (x2 :: _ as l2) when cmp x1 x2 <= 0 -> 
	merge (x1 :: acc) (r1, l2)
    | l1, x2 :: r2 ->
	merge (x2 :: acc) (l1, r2)
  in
  let r = merge [] (l1, l2) in
  (* printf "done@."; *)
  r

(* sort a list of strings in reverse order *)

let sort l = List.sort cmp l

(* sorting the lists of lists using map/reduce *)

let l = map_fold_ac ~f:sort ~fold:merge [] chunks
(* let () = List.iter (fun s -> printf "%s@." s) l *)

let () = assert (List.length l = n)

let () = 
  let rec check = function
    | [] | [_] -> ()
    | x :: (y :: _ as l) -> assert (cmp x y <= 0); check l
  in
  check l

(*
Local Variables: 
compile-command: "make -C ../.. tests/sort/a.out"
End: 
*)
