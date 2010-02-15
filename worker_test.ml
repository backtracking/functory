
open Format
open Unix
open Mapreduce

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

let () = Network.Worker.register_computation2 "reduce" (^)

let _ = Network.Worker.compute ~stop:false ~port ()

let () = printf "it works!@."

