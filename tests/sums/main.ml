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

(* open Functory.Sequential *)

let () = Functory.Control.set_debug true

open Functory.Cores
let () = set_number_of_cores 2

open Functory.Network
let () = declare_workers ~n:4 "moloch"
let () = declare_workers ~n:2 "orcus"

let f x = x+1

let n = int_of_string Sys.argv.(1)
let tasks = let l = ref [] in for i = 0 to n do l := (i,()) :: !l done; !l
let s = ref 0
let master _ fi = s := !s + fi; []
let () = compute ~worker:f ~master tasks


let fold = (+)

let () =
  let l = [1;2;3;4;5] and r = 20 in
  assert (map f l = [2;3;4;5;6]);
  assert (map_local_fold ~map:f ~fold 0 l = r);
  assert (map_remote_fold ~map:f ~fold 0 l = r);
  assert (map_fold_ac ~map:f ~fold 0 l = r);
(*   assert (map_fold_a ~map:f ~fold 0 l = r); *)
  ()

let f s = s ^ "."

let fold = (^)

let () =
  let l = ["a"; "bb"; "ccc"; "dddd"] in
  assert (map f l = ["a."; "bb."; "ccc."; "dddd."]);
  let check r = 
    String.length r = 14 &&
    List.for_all 
      (fun x -> 
	 let i = String.index r x.[0] in 
	 let n = String.length x in 
	 String.sub r i n = x && r.[i + n] = '.')
      l
  in
  assert (check (map_local_fold ~map:f ~fold "" l));
(*   assert (check (Str.map_local_fold ~map:f ~fold "" l)); *)
  assert (check (map_remote_fold ~map:f ~fold "" l));
(*   assert (check (Str.map_remote_fold ~map:f ~fold "" l)); *)
  assert (check (map_fold_ac ~map:f ~fold "" l));
(*   assert (check (Str.map_fold_ac ~map:f ~fold "" l)); *)
  assert (map_fold_a ~map:f ~fold "" l = "a.bb.ccc.dddd.");
  ()


(*
Local Variables: 
compile-command: "make -C ../.. tests/sums/a.out"
End: 
*)
