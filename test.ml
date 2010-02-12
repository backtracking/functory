
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
let () = declare_workers ~n:2 "129.175.4.107"

let rec compute x = if x <= 1 then 1 else x * compute (x-1)

let l = map compute [1;2;3;4]

let () = List.iter (fun s -> printf "%d@." s) l; printf "---@."

let l = map compute l

let () = List.iter (fun s -> printf "%d@." s) l; printf "---@."


let compute s = 
  let rec fib n = if n <= 1 then 1 else fib (n-1) + fib (n-2) in
  string_of_int (fib (int_of_string s))

let l = map compute ["10"; "20"; "15"] 

let () = List.iter (fun s -> printf "%s@." s) l; printf "---@."

let l = String.map compute ["10"; "20"; "15"] 

let () = List.iter (fun s -> printf "%s@." s) l; printf "---@."

let l = map compute ["5"; "6"; "7"; "8"; "9"; "10"; "11"; "12"; "13";
		     "20"; "30"; "40"; ] 

let () = List.iter (fun s -> printf "%s@." s) l
