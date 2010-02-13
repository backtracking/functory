
(* sorting lists of strings *)

open Format

open Mapreduce.Simple

let p = int_of_string Sys.argv.(1) (* number of parts *)
let n = int_of_string Sys.argv.(2) (* number of strings in each part *)
let k = int_of_string Sys.argv.(3) (* length of each string *)

let () = 
  printf "sorting %d lists of %d strings each (each string has length %d)@." 
    p n k;
  printf "creating lists...@?"

let create_string () =
  let s = String.create k in
  for i = 0 to k-1 do
    s.[i] <- Char.chr (97 + Random.int 26)
  done;
  s

let create_list _ =
  let rec make acc i = 
    if i = 0 then acc else make (create_string () :: acc) (i-1)
  in
  make [] n

let lists = Array.init p create_list
let () = printf "done@."

(*
Local Variables: 
compile-command: "make -C ../.. tests/sort/a.out"
End: 
*)
