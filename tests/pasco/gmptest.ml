
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
