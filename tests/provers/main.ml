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

(* open Functory.Sequential *)

(* open Functory.Cores *)
(* let () = set_number_of_cores 2 *)

open Functory
(* let () = Network.declare_workers ~n:1 "localhost" *)
let () = Network.declare_workers ~n:8 "moloch"
let () = Network.declare_workers ~n:4 "orcus"
let () = Network.declare_workers ~n:8 "belzebuth"

let directories = ref []
let add_directory d = directories := d :: !directories

let provers = ref []
let add_prover = function
  | "alt-ergo" | "simplify" | "z3" | "cvc3" as p -> provers := p :: !provers
  | p -> eprintf "unknown prover %s@." p; exit 1

let timeout = ref 10
let is_worker = ref false
let debug = ref false

let () = 
  Arg.parse
    ["-p", Arg.String add_prover, "<name> add a prover";
     "-timeout", Arg.Set_int timeout, "<seconds> set the timeout";
     "-w", Arg.Set is_worker, "run as a worker";
     "-debug", Arg.Set debug, "set the debug flag"; 
    ]
    add_directory
    "usage: provers [options] dir1 dir2 ..."

let () = if not !is_worker then Functory.Control.set_debug true

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
       List.fold_left (fun acc p -> ((f, p, !timeout), ()) :: acc) acc !provers)
    [] files

let () = printf "%d tasks@." (List.length tasks)

type result = 
  | Valid
  | Invalid
  | CannotDecide
  | Timeout
  | Failure

let prover_filename file = function
  | "alt-ergo" -> file
  | "simplify" -> Filename.chop_suffix file ".why" ^ ".sx"
  | "z3" | "cvc3" -> Filename.chop_suffix file ".why" ^ ".smt"
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
  let tmp = Filename.temp_file "tmp" ".out" in
  let s = sprintf "%s > %s" s tmp in
  if !debug then printf "+ %s@." s;
  ignore (Sys.command s);
  let cin = open_in tmp in
  let output =
    let b = Buffer.create 1024 in
    try while true do
      Buffer.add_string b (input_line cin);
      Buffer.add_char b '\n';
    done; assert false
    with End_of_file -> Buffer.contents b in
  close_in cin;
  Sys.remove tmp;
  output

let why_dp = "why-dp -simple"

let call_prover file p timeout =
  let s = match p with
    | "alt-ergo" 
    | "simplify" -> 
	sprintf "%s -timeout %d %s" why_dp timeout file
    | "z3" -> 
	sprintf "%s -smt-solver z3 -timeout %d %s" why_dp timeout file
    | "cvc3" -> 
	sprintf "%s -smt-solver cvc3 -timeout %d %s" why_dp timeout file
    | _ -> 
	assert false
  in
  command_with_output s

let interp_result s =
  try Scanf.sscanf s "Valid %f" (fun f -> Valid, f)
  with Scanf.Scan_failure _ ->
  try Scanf.sscanf s "Invalid %f" (fun f -> Invalid, f)
  with Scanf.Scan_failure _ ->
  try Scanf.sscanf s "I don't know %f" (fun f -> CannotDecide, f)
  with Scanf.Scan_failure _ ->
  try Scanf.sscanf s "Timeout %f" (fun f -> Timeout, f)
  with Scanf.Scan_failure _ ->
  try Scanf.sscanf s "Fail %f" (fun f -> Failure, f)
  with Scanf.Scan_failure _ ->
    eprintf "cannot interpret result %S@." s; Failure, 0.
 
let worker (file, p, timeout) =
  if !debug then printf "checking file %s with prover %s@." file p;
  let file' = prover_file file p in
  let res = call_prover file' p timeout in
  printf "@[<hov 2>file=%S@\noutput=%S@]@." file res;
  let r = interp_result res in
  r

type total = {
  mutable n : int;
  mutable time : float;
}

let create_total () = { n = 0; time = 0. }

type prover_results = {
  valid   : total;
  invalid : total;
  cannot  : total;
  timeout : total;
  failure : total;
}

let prover_table = Hashtbl.create 17
let () = 
  let add_prover p =
    let pr = { 
      valid   = create_total ();
      invalid = create_total ();
      cannot  = create_total ();
      timeout = create_total ();
      failure = create_total ();
    }
    in
    Hashtbl.add prover_table p pr
  in
  List.iter add_prover !provers

let master ((file, p, _), ()) (res, time) =
  printf "received time %f for file %s with prover %s@." time file p;
  let pr = Hashtbl.find prover_table p in
  let t = match res with
    | Valid -> pr.valid
    | Invalid -> pr.invalid
    | CannotDecide -> pr.cannot
    | Timeout -> pr.timeout
    | Failure -> pr.failure
  in
  t.n <- t.n + 1;
  t.time <- t.time +. time;
  []

let rec print_time fmt t =
  if t < 60. then 
    fprintf fmt "%.2f seconds" t
  else if t < 3600. then 
    let m = floor (t /. 60.) in
    fprintf fmt "%.0f minutes %a" m print_time (t -. 60. *. m)
  else 
    let h = floor (t /. 3600.) in
    fprintf fmt "%.0f hours %a" h print_time (t -. 3600. *. h)

let () = 
  let t0 = Unix.time () in
  if !is_worker then
    Network.Poly.Worker.compute worker ()
  else
    Network.Poly.Master.compute ~master tasks;
  let time_consumed = Unix.time () -. t0 in
  let sequential_time = ref 0. in
  let print_total t =
    printf "%3d (%5.2fs) | " t.n t.time;
    sequential_time := !sequential_time +. t.time
  in
  let print p pr =
    printf "%10s | " p;
    print_total pr.valid;
    print_total pr.invalid;
    print_total pr.cannot;
    print_total pr.timeout;
    print_total pr.failure;
    printf "@."
  in
  printf "%d tasks@." (List.length tasks);
  printf "           |  valid       | invalid      | cannot       | timeout      | failure@.";
  printf "---------------------------------------------------------------------------------------@.";
  Hashtbl.iter print prover_table;
  printf "@.";
  printf "time consumed  : %a@." print_time time_consumed;
  printf "sequential time: %a@." print_time !sequential_time;
  printf "ratio is %.2f@." (!sequential_time /. time_consumed)


(*
Local Variables: 
compile-command: "make -C ../.. tests/provers/a.out"
End: 
*)


