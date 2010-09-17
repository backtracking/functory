
open Format
open Functory.Sequential

(* Network *)
(* let () = declare_workers ~n:12 "localhost" *)
(* open Same *)

open Gmp.Z

let read_matrix ?(by_column=false) file =
  let c = open_in file in
  let s = input_line c in
  let n, m = Scanf.sscanf s "%d %d" (fun n m -> n, m) in
  printf "n=%d, m=%d@." n m;
  let mat = 
    (if by_column then Array.create_matrix m n else Array.create_matrix n m)
    zero
  in
  for i = 0 to n-1 do for j = 0 to m-1 do
    let x = from_string (input_line c) in
    if by_column then mat.(j).(i) <- x else mat.(i).(j) <- x
  done done;
  close_in c;
  mat

let a = read_matrix Sys.argv.(1)
let b = read_matrix ~by_column:true Sys.argv.(2)

let naive_multiplication a b =
  let n = Array.length a in
  assert (n > 0);
  let m = Array.length a.(0) in
  assert (m = Array.length b.(0));
  let p = Array.length b in
  let c = Array.create_matrix n p zero in
  for i = 0 to n-1 do
    for j = 0 to p-1 do
      for k = 0 to m-1 do
	c.(i).(j) <- add c.(i).(j) (mul a.(i).(k) b.(j).(k))
      done
    done
  done;
  c

let _ = naive_multiplication a b

(* let worker = fib_ui *)

(* let tasks = [100000, ()] *)

(* let master _ r = *)
(*   Format.printf "result = %a@." print r; *)
(*   [] *)

(* let () = compute ~worker ~master tasks *)


(*
Local Variables: 
compile-command: "make -C ../.. tests/pasco/matmul"
End: 
*)
