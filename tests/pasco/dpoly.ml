
(* open Functory.Sequential *)
open Functory.Cores
let () = set_number_of_cores 7
let computation = ref 0.

module type RING = sig
  type t
  val zero : t
  val one : t
  val plus : t -> t -> t
  val mult : t -> t -> t
  val eq : t -> t -> bool
  val print : Format.formatter -> t -> unit
end

module type POLYNOMIAL = sig
  type c (* type of numbers used in the polynomial *)
  type t (* type of the polynomials *)
  val zero : t
  val one : t
  val monom : c -> int -> t
  val plus : t -> t -> t
  val mult : t -> t -> t
  val eq : t -> t -> bool
  val print : Format.formatter -> t -> unit
  val eval : t -> c -> c
end

module MakePoly (A: RING) : POLYNOMIAL
  with type c = A.t = 
struct
  type c = A.t
  type monom = (c * int) (* (coeff, power) *)
  type t = monom list
  let zero = []
  let one = [A.one, 0]
  let rec eq p1 p2 = 
    match p1, p2 with
	[], [] -> true
      | (a1, x1) :: q1, (a2, x2) :: q2 -> 
	  (x1 = x2) && A.eq a1 a2 && eq q1 q2
      | _ -> false
  let monom a x = 
    if x < 0 then
      failwith "fail monom: power can not be negative";
    if A.eq a A.zero then
      []
    else
      [(a, x)]
  let rec plus p1 p2 = match p1, p2 with 
    | (a1, x1 as m1) :: q1, (a2, x2 as m2) :: q2 -> 
	if x1 < x2 then
	  m1 :: plus q1 p2
	else if x1 = x2 then
	  let a = A.plus a1 a2 in 
	  if A.eq a A.zero then plus q1 q2
	  else (a, x1) :: plus q1 q2
	else
	  m2 :: plus p1 q2
    | [], _ -> p2
    | _, [] -> p1

  let rec times (a, x as m) p = 
    (* assume a <> 0 *)
    match p with 
      | [] -> []
      | (a1, x1) :: q ->
	  let a2 = A.mult a a1 in 
	  if A.eq a2 A.zero then times m q
	  else (a2, x + x1) :: times m q

  let mult p = List.fold_left (fun r m -> plus r (times m p)) zero

  (* map_fold_ac or map_local_fold in not efficient enoug: too many tasks *)

  let t = 7
  (* optimal when equal to the number of cores 
     best result: 4 times speedup (6 or 7 cores on moloch) with mult2 *)

  let split_in n l =
    let a = Array.create n [] in
    let rec fill i = function
      | [] -> List.map List.rev (Array.to_list a)
      | m :: p -> a.(i) <- m :: a.(i); fill ((i+1) mod n) p
    in
    fill 0 l

  let time f x =                                                   
    let u = (Unix.times()).Unix.tms_utime in                                  
    let y = f x in
    let ut = (Unix.times()).Unix.tms_utime -. u in
    y, ut

  (* solution 1: split p1 into t pieces, and multiply each by p2 *)
  let mult1 p1 p2 =
    let map s1 = 
      let p = mult s1 p2 in
      Format.printf "mult1: %d x %d -> %d@." (List.length s1) (List.length p2)
      	(List.length p);
      p
    in
    let fold = plus in
    map_fold_ac ~map ~fold zero (split_in t p1)

  (* solution 2: split both p1 and p2 in sqrt(t) pieces, and multiply *)
  let mult2 p1 p2 =
    let s = truncate (ceil (sqrt (float t))) in
    let l1 = split_in s p1 in
    let l2 = split_in s p2 in
    let tl = 
      List.flatten (List.map (fun p1 -> List.map (fun p2 -> p1,p2) l2) l1) 
    in
    Format.printf "mult2: %d tasks@." (List.length tl);
    let map (s1, s2) = 
      let p = mult s1 s2 in
      Format.printf "mult2: %d x %d -> %d@." (List.length s1) (List.length s2)
      	(List.length p);
      p
    in
    let fold = plus in
    map_fold_ac ~map ~fold zero tl

  let mult = mult2

  let rec pow c x = match x with
      (* given c, x calculates c^x *)
    | 0 -> A.one 
    | 1 -> c
    | x ->
	let l = pow c (x/2) in 
	let l2 = A.mult l l in
	  if x mod 2 = 0 then l2 else A.mult c l2

  let eval p c = 
    let rec eval_loop acc = function
      | [] ->
	  acc
      | [a0, x0] ->
	  A.mult (pow c x0) (A.plus a0 acc)
      | (an, xn) :: ((an_1, xn_1) :: _ as q) ->
	  eval_loop (A.mult (pow c (xn - xn_1)) (A.plus an acc)) q
    in
    eval_loop A.zero (List.rev p)

  (* with continuations -> not efficient *)
  let evalk p c =
    let rec eval_loop x k = function
      | [] ->
	  k A.zero
      | (a0, x0) :: p ->
	  eval_loop x0 (fun r -> k (A.mult (pow c (x0-x)) (A.plus a0 r))) p
    in
    eval_loop 0 (fun r -> r) p

(* match List.rev p with *)
(*       [] -> A.zero *)
(*     | (h :: t) ->    *)
(* 	let dm (a1, x1) (a2, x2) = *)
(* 	  A.plus (A.mult (pow c (x1 - 1)) a1) a2, x2 *)
(* 	in *)
(* 	let a1, x2 = List.fold_left dm h t in  *)
(* 	  A.mult (pow c x2) a1  *)

  open Format

  let print fmt p =
    fprintf fmt "(";
    let b = List.fold_left 
      (fun acc (a,k) -> 
         (* acc is false for the first monom *) 
          if acc then fprintf fmt " + ";
          fprintf fmt "%ax^%d" A.print a k;
          true
      ) false p in
    if (not b) then (A.print fmt A.zero);
    fprintf fmt ")"
end


module IntRing = 
struct
  type t = int
  let zero = 0
  let one = 1
  let plus a b = a + b
  let mult a b = a * b
  let eq a b = (a = b)
  let print = Format.pp_print_int
end

open Gmp.Z

module GmpRing = struct
  type t = Gmp.Z.t
  let zero = zero
  let one = one
  let plus = add
  let mult = mul
  let eq = equal
  let print = print
end

module P = MakePoly (GmpRing)

open Format

(* let two = Gmp.Z.of_int 2 *)
(* let p1 = P.plus (P.monom one 0) (P.monom one 2) (\* 1+X^2 *\) *)
(* let () = printf "%a@." P.print p1 *)
(* let p2 = P.mult p1 p1 *)
(* let () = printf "P2 = %a@." P.print p2 *)
(* let () = printf "P2(2) = %a@." print (P.eval p2 two) *)

let rec random_poly acc = function
  | -1 -> acc
  | n -> random_poly (P.plus (P.monom (of_int (Random.int 1000)) n) acc) (n-1)

let p1 = random_poly P.zero 3_000
let p2 = random_poly P.zero 3_000
(* let () = printf "(P1 + P2)(2) = %a@." print (P.eval (P.mult p1 p2) two) *)

let () = for i = 1 to 10 do ignore (P.mult p1 p2) done
let () = Format.printf "computation time: %f@." !computation

(*
Local Variables: 
compile-command: "make -C ../.. tests/pasco/dpoly"
End: 
*)