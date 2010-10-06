
open Format

(* open Functory.Sequential *)

(* open Functory.Cores *)
(* let () = set_number_of_cores 8 *)

(* open Functory.Network *)
(* let () = declare_workers ~n:8 "localhost" *)
(* open Same *)

open Dpoly
open Gmp.Z

let read_poly file =
  let c = open_in file in
  let _ = input_line c in
  let p = ref G.zero in
  try
    while true do
      let s = input_line c in
      Scanf.sscanf s "%d %s" 
	(fun n m -> p := G.plus (G.monom (from_string m) n) !p)
    done;
    assert false
  with End_of_file ->
    close_in c;
    !p

let print_poly p =
  let l = G.to_list p in
  printf "%d@\n" (List.length l);
  let print1 (c, d) = printf "%d %a@\n" d print c in
  List.iter print1 l;
  printf "@?"

let u = read_poly Sys.argv.(1)
(* let () = eprintf "u = %a@\n@." G.print u *)
let v = read_poly Sys.argv.(2)
(* let () = eprintf "v = %a@\n@." G.print v *)

let g = G.dgcd u v
(* let () = eprintf "gcd(u,v) = %a@." G.print g *)
let () = print_poly g

(*
Local Variables: 
compile-command: "make -C ../.. tests/pasco/gcdpol"
End: 
*)
