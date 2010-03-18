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

(* let () = Mapreduce.Control.set_debug true *)
open Mapreduce
let () = Network.declare_workers ~n:1 "localhost"
let () = Network.declare_workers ~n:12 "moloch"
(* let () = Network.declare_workers ~n:4 "orcus" *)
let () = Network.declare_workers ~n:4 "belzebuth"

let directories = ref []
let add_directory d = directories := d :: !directories

let provers = ref []
let add_prover = function
  | "alt-ergo" | "simplify" | "z3" as p -> provers := p :: !provers
  | p -> eprintf "unknown prover %s@." p; exit 1

let timeout = ref 10

let () = 
  Arg.parse
    ["-p", Arg.String add_prover, "<name> add a prover";
     "-timeout", Arg.Set_int timeout, "<seconds> set the timeout";
    ]
    add_directory
    "usage: provers [options] dir1 dir2 ..."

let files = 
  let files = ref [] in
  let scan_directory d =
    if not (Sys.is_directory d) then begin
      eprintf "%s is not a directory@." d; exit 1
    end;
    let d = 
      if Filename.is_relative d then 
	Filename.concat (Sys.getcwd ()) d 
      else 
	d
    in
    let add_file f = 
      if Filename.check_suffix f ".why" then
	files := Filename.concat d f :: !files
    in
    Array.iter add_file (Sys.readdir d);
  in
  List.iter scan_directory !directories;
  !files

let () = 
  printf "%d files@." (List.length files);
  (*   List.iter (fun f -> printf "%s@." f) files *)
  ()

let tasks =
  List.fold_left
    (fun acc f ->
       List.fold_left (fun acc p -> ((f, p), ()) :: acc) acc !provers)
    [] files

let () =
  printf "%d tasks@." (List.length tasks)

type result = 
  | Valid
  | Invalid
  | IDontKnow
  | Failure

let prover_filename file = function
  | "alt-ergo" -> file
  | "simplify" -> Filename.chop_suffix file ".why" ^ ".sx"
  | "z3" -> Filename.chop_suffix file ".why" ^ ".smt"
  | _ -> assert false

let make_file file file' = 
  let cmd = 
    if Filename.check_suffix file' ".sx" then
      sprintf "why --no-pervasives --simplify %s -o %s" file file' 
    else if Filename.check_suffix file' ".smt" then
      sprintf "why --no-pervasives --smtlib %s -o %s" file file' 
    else begin
      eprintf "don't know how to build file %s@." file'; exit 1
    end
  in
  ignore (Sys.command cmd)

let prover_file file p = 
  let file' = prover_filename file p in
  if not (Sys.file_exists file') then make_file file file';
  file'

let command_with_output s =
  printf "+ %s@." s;
  let cin = Unix.open_process_in s in
  let output =
    let b = Buffer.create 1024 in
    try while true do
      Buffer.add_string b (input_line cin);
      Buffer.add_char b '\n';
    done; assert false
    with End_of_file -> Buffer.contents b in
  close_in cin;
  printf "%s@." output;
  output

let call_prover file p =
  let s = match p with
    | "alt-ergo" 
    | "simplify" -> sprintf "why-dp -simple %s" file
    | "z3"       -> sprintf "why-dp -simple -smt-solver z3 %s" file
    | _ -> assert false
  in
  command_with_output s

let interp_result s =
  Valid, 0.

let worker (file, p) =
  printf "checking file %s with prover %s@." file p;
  let file' = prover_file file p in
  let res = call_prover file' p in
  interp_result res

let master ((file, p), ()) (res, time) =
  printf "received time %f for file %s with prover %s@." time file p;
  []

let () = Network.Same.compute ~worker ~master tasks

(*
Local Variables: 
compile-command: "make -C ../.. tests/provers/a.out"
End: 
*)


