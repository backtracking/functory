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

open Format

(* outputs n lines of k alphabetic characters *)

let n = int_of_string Sys.argv.(1) (* total number of strings *)
let k = int_of_string Sys.argv.(2) (* length of each string *)

let create_string () =
  let s = String.create k in
  for i = 0 to k-1 do
    s.[i] <- Char.chr (97 + Random.int 26)
  done;
  s

let () =
  for i = 1 to n do
    printf "%s@." (create_string ())
  done
