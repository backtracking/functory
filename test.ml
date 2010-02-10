
open Format
open Mapreduce.Network
let () = declare_workers ~n:4 "moloch"

let compute s = 
  let rec fib n = if n <= 1 then 1 else fib (n-1) + fib (n-2) in
  string_of_int (fib (int_of_string s))

let l = map compute ["10"; "20"; "15"] 

let () = List.iter (fun s -> printf "%s@." s) l; printf "---@."

let l = map compute ["5"; "6"; "7"; "8"; "9"; "10";] 

let () = List.iter (fun s -> printf "%s@." s) l


