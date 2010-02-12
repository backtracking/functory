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

(* open Mapreduce.Simple *)

let () = Mapreduce.Control.set_debug true

(* open Mapreduce.Cores *)
(* let () = set_number_of_cores 2 *)

open Mapreduce.Network
let () = declare_workers ~n:4 "moloch"
let () = declare_workers ~n:2 "129.175.4.107"

let f x = x+1

let reduce = (+)

let () =
  let l = [1;2;3;4;5] and r = 20 in
  assert (map f l = [2;3;4;5;6]);
  assert (map_local_reduce ~map:f ~reduce 0 l = r);
  assert (map_remote_reduce ~map:f ~reduce 0 l = r);
  assert (map_reduce_ac ~map:f ~reduce 0 l = r);
(*   assert (map_reduce_a ~map:f ~reduce 0 l = r); *)
  ()

let f s = s ^ "."

let reduce = (^)

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
  assert (check (map_local_reduce ~map:f ~reduce "" l));
  assert (check (Str.map_local_reduce ~map:f ~reduce "" l));
  assert (check (map_remote_reduce ~map:f ~reduce "" l));
  assert (check (Str.map_remote_reduce ~map:f ~reduce "" l));
  assert (check (map_reduce_ac ~map:f ~reduce "" l));
  assert (check (Str.map_reduce_ac ~map:f ~reduce "" l));
(*   assert (map_reduce_a ~map:f ~reduce "" l = "a.bb.ccc.dddd."); *)
  ()


(*
Local Variables: 
compile-command: "make -C ../.. tests/sums/a.out"
End: 
*)
