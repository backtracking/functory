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

(* sorting a file using map/reduce *)

open Format

(* let () = Functory.Control.set_debug true *)

open Functory.Sequential

(* open Functory.Cores *)
(* let () = Functory.Cores.set_number_of_cores 10 *)

(* open Functory.Network *)
(* let () = declare_workers ~n:12 "moloch" *)
(* let () = declare_workers ~n:4 "orcus" *)

let file = Sys.argv.(1)
let s = int_of_string Sys.argv.(2) (* chunk size *)

let () = 
  printf "reading file '%s'... @?" file

let read_file file =
  let c = open_in file in
  let rec read acc =
    let l = try Some (input_line c) with End_of_file -> None in
    match l with None -> close_in c; List.rev acc | Some l -> read (l :: acc)
  in
  read []

let empty_file = "/users/demons/filliatr/tmp/empty-file"

let remove_file f =
  if f <> empty_file && Sys.file_exists f then 
    Sys.remove f

(* reading initial file and splitting it into chunks *)

let list = read_file file

let n = List.length list
let () = printf "done@.%d lines@." n

let rec create_chunk acc i = function
  | [] -> acc, []
  | l when i = 0 -> acc, l
  | x :: l -> create_chunk (x :: acc) (i - 1) l

let rec split acc l =
  if l = [] then acc 
  else let c, l = create_chunk [] s l in split (c :: acc) l

let chunks = split [] list

(* merging lists which are sorted in reverse order (into a sorted list) *)

let cmp = String.compare

(* let merge = List.merge cmp *)

let merge_list l1 l2 =
  (* printf "merging %d %d...@." (List.length l1) (List.length l2); *)
  let rec merge acc = function
    | [], l | l, [] -> 
	List.rev_append acc l
    | x1 :: r1, (x2 :: _ as l2) when cmp x1 x2 <= 0 -> 
	merge (x1 :: acc) (r1, l2)
    | l1, x2 :: r2 ->
	merge (x2 :: acc) (l1, r2)
  in
  let r = merge [] (l1, l2) in
  (* printf "done@."; *)
  r

let tmp_file = 
  let r = ref 0 in
  fun () -> 
    incr r; 
    sprintf "/users/demons/filliatr/tmp/sort-%s-%d-%d" 
      (Unix.gethostname ()) (Unix.getpid ()) !r

let write_file l f =
  let c = open_out f in
  List.iter (fun s -> output_string c s; output_char c '\n') l;
  close_out c

let write_in_tmp_file l =
  let tmp = tmp_file () in
  write_file l tmp;
  tmp

let merge f1 f2 =
  let l1 = read_file f1 in
  let l2 = read_file f2 in
  remove_file f1; 
  remove_file f2;
  let l = merge_list l1 l2 in
  write_in_tmp_file l

(* sort a list of strings in reverse order *)

let sort_list l = List.sort cmp l

let sort f =
  let l = read_file f in
  remove_file f;
  let l = sort_list l in
  write_in_tmp_file l

(* sorting the lists of lists using map/reduce *)

let chunk_files = List.map write_in_tmp_file chunks
let f = map_fold_ac ~f:sort ~fold:merge empty_file chunk_files
let () = List.iter remove_file chunk_files
let () = printf "final file is '%s'@." f

let final_list = read_file f

let () = assert (List.length final_list = n)

let () = 
  let rec check = function
    | [] | [_] -> ()
    | x :: (y :: _ as l) -> assert (cmp x y <= 0); check l
  in
  check final_list

(*
Local Variables: 
compile-command: "make -C ../.. tests/sort-nfs/a.out"
End: 
*)
