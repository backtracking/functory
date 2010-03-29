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
open Unix
open Functory

let port = ref 51000
let () = 
  Arg.parse
    ["-port", Arg.Set_int port, "<n>  sets the port number";
     "-d", Arg.Unit (fun () -> Control.set_debug true), "sets the debug flag";]
    (fun _ -> ())
    "worker_test: usage:"
let port = !port

let compute s = 
  let rec fib n = if n <= 1 then 1 else fib (n-1) + fib (n-2) in
  string_of_int (fib (int_of_string s))

let () = Network.Worker.register_computation "f" compute

let succ s = string_of_int (succ (int_of_string s))

let () = Network.Worker.register_computation "map" succ

let add s1 s2 = string_of_int (int_of_string s1 + int_of_string s2)

let () = Network.Worker.register_computation2 "fold" (^)

let _ = Network.Worker.compute ~stop:false ~port ()

let () = printf "it works!@."

