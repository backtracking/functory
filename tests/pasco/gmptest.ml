(**************************************************************************)
(*                                                                        *)
(*  Functory: a distributed computing library for OCaml                   *)
(*  Copyright (C) 2010- Jean-Christophe Filliatre and Kalyan Krishnamani  *)
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

open Functory.Network
let () = declare_workers ~n:12 "localhost"
open Same

open Gmp.Z

let worker = fib_ui

let tasks = [100000, ()]

let master _ r =
  Format.printf "result = %a@." print r;
  []

let () = compute ~worker ~master tasks


(*
Local Variables: 
compile-command: "make -C ../.. tests/pasco/gmptest"
End: 
*)
