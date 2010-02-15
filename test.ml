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

let () = 
  Arg.parse
    ["-d", Arg.Unit (fun () -> Mapreduce.Control.set_debug true), 
     "sets the debug flag";]
    (fun _ -> ())
    "test: usage:"

(* open Mapreduce.Simple *)

(* open Mapreduce.Cores *)
(* let () = set_number_of_cores 4 *)

open Mapreduce.Network
let () = declare_workers ~n:4 "moloch"
let () = declare_workers ~n:2 "orcus"

let f x = x+1

let reduce = (+)

let () =
  let l = [1;2;3;4;5] and r = 20 in
  printf "map@.";
  assert (map f l = [2;3;4;5;6]);
  printf "map_local_reduce@.";
  assert (map_local_reduce ~map:f ~reduce 0 l = r);
  printf "map_remote_reduce@.";
  assert (map_remote_reduce ~map:f ~reduce 0 l = r);
  printf "map_reduce_ac@.";
  assert (map_reduce_ac ~map:f ~reduce 0 l = r);
  printf "map_reduce_a@.";
  assert (map_reduce_a ~map:f ~reduce 0 l = r);
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
  printf "map_local_reduce@.";
  assert (check (map_local_reduce ~map:f ~reduce "" l));
  assert (check (Str.map_local_reduce ~map:f ~reduce "" l));
  printf "map_remote_reduce@.";
  assert (check (map_remote_reduce ~map:f ~reduce "" l));
  assert (check (Str.map_remote_reduce ~map:f ~reduce "" l));
  printf "map_reduce_ac@.";
  assert (check (map_reduce_ac ~map:f ~reduce "" l));
  assert (check (Str.map_reduce_ac ~map:f ~reduce "" l));
  printf "map_reduce_a@.";
  assert (map_reduce_a ~map:f ~reduce "" l = "a.bb.ccc.dddd.");
  assert (Str.map_reduce_a ~map:f ~reduce "" l = "a.bb.ccc.dddd.");
  ()


(***********

let rec compute x = if x <= 1 then 1 else x * compute (x-1)

(* let n = map_reduce_a ~map:compute ~reduce:(+) 0 [1;2;3;4;5;6;7;8;9] *)

(* let () = printf "%d@." n *)

let f x = sprintf ".%d." x

let s = map_reduce_a ~map:f ~reduce:(^) "" [1;2;3;4;5;6;7]

let () = printf "%s@." s; exit 0



let l = map compute [1;2;3;4]

let () = List.iter (fun s -> printf "%d@." s) l; printf "---@."

let l = map compute l

let () = List.iter (fun s -> printf "%d@." s) l; printf "---@."


let compute s = 
  let rec fib n = if n <= 1 then 1 else fib (n-1) + fib (n-2) in
  string_of_int (fib (int_of_string s))

let l = map compute ["10"; "20"; "15"] 

let () = List.iter (fun s -> printf "%s@." s) l; printf "---@."

let l = map compute ["10"; "20"; "15"] 

let () = List.iter (fun s -> printf "%s@." s) l; printf "---@."

let l = map compute ["5"; "6"; "7"; "8"; "9"; "10"; "11"; "12"; "13";
		     "20"; "30"; "40"; ] 

let () = List.iter (fun s -> printf "%s@." s) l
************)
