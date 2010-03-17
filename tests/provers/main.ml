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

open Format

(* open Mapreduce.Sequential *)

(* open Mapreduce.Cores *)
(* let () = set_number_of_cores 2 *)

let () = Mapreduce.Control.set_debug true
open Mapreduce
let () = Network.declare_workers ~n:1 "localhost"
(* let () = Network.declare_workers ~n:12 "moloch" *)
(* let () = Network.declare_workers ~n:4 "orcus" *)

let files = 
  if Array.length Sys.argv = 1 || not (Sys.is_directory Sys.argv.(1)) 
  then begin eprintf "usage: disco-provers <directory>"; exit 1 end;
  let dir = Sys.argv.(1) in
  let files = ref [] in
  let add f = 
    if Filename.check_suffix f ".why"  ||
       Filename.check_suffix f ".sx"  ||
       Filename.check_suffix f ".smt" 
    then
      files := Filename.concat dir f :: !files
  in
  Array.iter add (Sys.readdir dir);
  !files

let () = printf "%d files@." (List.length files)


(*
Local Variables: 
compile-command: "make -C ../.. tests/provers/a.out"
End: 
*)


