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
open Functory.Network
let () = Functory.Control.set_debug true
let () = declare_workers ~n:1 "moloch"
let () = declare_workers ~n:1 "localhost"

let map x =
  eprintf "task %d@." x;
  Unix.sleep (if x = 1 then 100 else 30);
  x

let () = assert (Same.map_local_fold ~f:map ~fold:(+) 0 [1; 2] = 3)

(*
Local Variables: 
compile-command: "unset LANG; make -C ../.. install-test"
End: 
*)
