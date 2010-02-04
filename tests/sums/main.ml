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

open Mapreduce.Cores
let () = set_number_of_cores 2
(* open Mapreduce.Simple *)

let map x = [x+1]

let reduce = (+)

let () =
  let r = fold ~map ~reduce 0 [1;2;3;4;5] in
  assert (r = 20);
  Format.printf "%d@." r


(*
Local Variables: 
compile-command: "make -C ../.. tests/sums/a.out"
End: 
*)
