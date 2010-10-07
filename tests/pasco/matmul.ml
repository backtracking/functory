
open Format

(* open Functory.Sequential *)

(* open Functory.Cores *)
(* let () = set_number_of_cores 6 *)

open Functory.Network
let () = declare_workers ~n:(int_of_string Sys.argv.(3)) "moloch"
(* let () = declare_workers ~n:6 "belzebuth" *)
(* let () = Functory.Control.set_debug true *)
open Same

open Gmp.Z

let read_matrix ?(by_column=false) file =
  let c = open_in file in
  let s = input_line c in
  let n, m = Scanf.sscanf s "%d %d" (fun n m -> n, m) in
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

let parall_multiplication a b =
  let n = Array.length a in
  assert (n > 0);
  let m = Array.length a.(0) in
  assert (m = Array.length b.(0));
  let p = Array.length b in
  let c = Array.create_matrix n p zero in
  let tasks = ref [] in
  for i = 0 to n-1 do for j = 0 to p-1 do
    tasks := ((a.(i), b.(j)), (i,j)) :: !tasks
  done done;
  let worker (ai, bj) =
    let c = ref zero in
    for k = 0 to m-1 do
	c := add !c (mul ai.(k) bj.(k))
    done;
    !c
  in
  let master (_, (i,j)) r = c.(i).(j) <- r; [] in
  compute ~worker ~master !tasks;
  c

let parall_line_multiplication a b =
  let n = Array.length a in
  assert (n > 0);
  let m = Array.length a.(0) in
  assert (m = Array.length b.(0));
  let p = Array.length b in
  let c = Array.create n [||] in
  let tasks = ref [] in
  for i = 0 to n-1 do tasks := (a.(i), i) :: !tasks done;
  let worker ai =
    let c = Array.create p zero in
    for j = 0 to p-1 do for k = 0 to m-1 do
      c.(j) <- add c.(j) (mul ai.(k) b.(j).(k))
    done done;
    c
  in
  let master (_, i) r = c.(i) <- r; [] in
  compute ~worker ~master !tasks;
  c

let dump c =
  let n = Array.length c in
  let m = Array.length c.(0) in
  printf "%d %d@." n m;
  let dump_coeff x = printf "%a@." print x in
  Array.iter (Array.iter dump_coeff) c

(* let _ = naive_multiplication a b *)
let c = match Sys.argv.(4) with
  | "mm1" -> parall_multiplication a b 
  | "mm2" -> parall_line_multiplication a b
  | _ -> assert false
(* let () = dump c *)
let () = printf "multiplication done@."

(*
Local Variables: 
compile-command: "make -C ../.. tests/pasco/matmul"
End: 
*)
