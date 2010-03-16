
open Format
open Mapreduce.Network
let () = Mapreduce.Control.set_debug true
let () = declare_workers ~n:1 "moloch"
let () = declare_workers ~n:1 "localhost"

let map x =
  eprintf "task %d@." x;
  Unix.sleep (if x = 1 then 100 else 30);
  x

let () = assert (Same.map_local_fold ~map ~fold:(+) 0 [1; 2] = 3)

(*
Local Variables: 
compile-command: "unset LANG; make -C ../.. install-test"
End: 
*)
