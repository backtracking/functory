
(* sorting lists of strings *)

open Format

open Mapreduce.Simple
open Mapreduce.Cores
let () = Mapreduce.Control.set_debug true
let () = Mapreduce.Cores.set_number_of_cores 2

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

let lists = Array.to_list (Array.init p create_list)
let () = printf "done@."

(* merging lists which are sorted in reverse order (into a sorted list) *)

let cmp = String.compare

(* let merge = List.merge cmp *)

let merge l1 l2 =
  printf "merging %d %d...@." (List.length l1) (List.length l2);
  let rec merge acc = function
    | [], l | l, [] -> 
	List.rev_append acc l
    | x1 :: r1, (x2 :: _ as l2) when cmp x1 x2 <= 0 -> 
	merge (x1 :: acc) (r1, l2)
    | l1, x2 :: r2 ->
	merge (x2 :: acc) (l1, r2)
  in
  let r = merge [] (l1, l2) in
  printf "done@.";
  r

(* sort a list of strings in reverse order *)

let sort l = List.sort cmp l

(* sorting the lists of lists using map/reduce *)

let l = map_reduce_ac ~map:sort ~reduce:merge [] lists
(* let () = List.iter (fun s -> printf "%s@." s) l *)
let () = 
  let rec check = function
    | [] | [_] -> ()
    | x :: (y :: _ as l) -> assert (cmp x y <= 0); check l
  in
  check l

(*
Local Variables: 
compile-command: "make -C ../.. tests/sort/a.out"
End: 
*)
