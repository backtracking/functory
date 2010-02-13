
open Format

(* outputs n lines of k alphabetic characters *)

let n = int_of_string Sys.argv.(1) (* total number of strings *)
let k = int_of_string Sys.argv.(2) (* length of each string *)

let create_string () =
  let s = String.create k in
  for i = 0 to k-1 do
    s.[i] <- Char.chr (97 + Random.int 26)
  done;
  s

let () =
  for i = 1 to n do
    printf "%s@." (create_string ())
  done
