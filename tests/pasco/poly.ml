module type RING = sig
  type t
  val zero : t
  val one : t
  val neg : t -> t
  val plus : t -> t -> t
  val mult : t -> t -> t
  val eq : t -> t -> bool
  val gcd : t -> t -> t
  val exact_div : t -> t -> t
  val div_mod : t -> t -> t * t
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
  val degree : t -> int
  val cont : t -> c
  val pseudo_rem : t -> t -> t
  val gcd : t -> t -> t (* Generalized Euclidean Algorithm *)
  val from_list : (c * int) list -> t
  val to_list : t -> (c * int) list
  val print : Format.formatter -> t -> unit
  val eval : t -> c -> c
end

module MakePoly (D: RING) : POLYNOMIAL with type c = D.t = 
struct
  type c = D.t
  type monom = (c * int) (* (coeff, power) *)
  type t = monom list (* sorted in decreasing order of degrees *)
  let zero = []
  let one = [D.one, 0]

  open Format

  let print fmt p =
    fprintf fmt "(";
    let b = List.fold_left 
      (fun acc (a,k) -> 
         (* acc is false for the first monom *) 
          if acc then fprintf fmt " + ";
          fprintf fmt "%ax^%d" D.print a k;
          true
      ) false p in
    if (not b) then (D.print fmt D.zero);
    fprintf fmt ")"

  let rec eq p1 p2 = 
    match p1, p2 with
	[], [] -> true
      | (a1, x1) :: q1, (a2, x2) :: q2 -> 
	  (x1 = x2) && D.eq a1 a2 && eq q1 q2
      | _ -> false

  let monom a x = 
    if x < 0 then
      failwith "fail monom: power can not be negative";
    if D.eq a D.zero then
      []
    else
      [(a, x)]

  let rec plus p1 p2 = match p1, p2 with 
    | (a1, x1 as m1) :: q1, (a2, x2 as m2) :: q2 -> 
	if x1 > x2 then
	  m1 :: plus q1 p2
	else if x1 = x2 then
	  let a = D.plus a1 a2 in 
	  if D.eq a D.zero then plus q1 q2
	  else (a, x1) :: plus q1 q2
	else
	  m2 :: plus p1 q2
    | [], _ -> p2
    | _, [] -> p1

  let from_list = List.fold_left (fun p (c, d) -> plus (monom c d) p) zero

  let to_list p = p

  let rec times (a, x as m) p = 
    (* assume a <> 0 *)
    match p with 
      | [] -> []
      | (a1, x1) :: q ->
	  let a2 = D.mult a a1 in 
	  if D.eq a2 D.zero then times m q
	  else (a2, x + x1) :: times m q

  let mult p = List.fold_left (fun r m -> plus r (times m p)) zero

  let rec pow c x = match x with
      (* given c, x calculates c^x *)
    | 0 -> D.one 
    | 1 -> c
    | x ->
	let l = pow c (x/2) in 
	let l2 = D.mult l l in
	if x mod 2 = 0 then l2 else D.mult c l2

  let eval p c = 
    let rec eval_loop acc = function
      | [] ->
	  acc
      | [a0, x0] ->
	  D.mult (pow c x0) (D.plus a0 acc)
      | (an, xn) :: ((an_1, xn_1) :: _ as q) ->
	  eval_loop (D.mult (pow c (xn - xn_1)) (D.plus an acc)) q
    in
    eval_loop D.zero p

  (* with continuations -> not efficient *)
  let evalk p c =
    let rec eval_loop x k = function
      | [] ->
	  k D.zero
      | (a0, x0) :: p ->
	  eval_loop x0 (fun r -> k (D.mult (pow c (x0-x)) (D.plus a0 r))) p
    in
    eval_loop 0 (fun r -> r) (List.rev p)

  (* gcd of all coefficients *)
  let cont p = 
    List.fold_left (fun cont (c, _) -> D.gcd cont c) D.zero p
      
  let exact_div c p = 
    let divide (x, d) = (D.exact_div x c, d) in
    List.map divide p
    
  let pp p = exact_div (cont p) p

  let degree = function
    | [] -> -1
    | (_, d) :: _ -> d

  let pseudo_rem u v =
    let vn, n = match v with
      | (vn, n) :: _ -> vn, n
      | [] -> invalid_arg "pseudo_div: v should be non zero"
    in
    let rec loop = function
      | u when degree u < n -> 
	u
      | (um, m) :: _ as u ->
	let u = times (vn, 0) u in
	let u = plus u (times (D.neg um, m - n) v) in
	loop u
      | [] ->
	assert false
    in
    loop u

  let gcd u v =
    let cu = cont u and cv = cont v in
    let u = exact_div cu u and v = exact_div cv v in
    let d = D.gcd cu cv in
    assert (not (D.eq d D.zero));
    let rec loop u v =
      (* Format.printf "deg(u)=%d deg(v)=%d@." (degree u) (degree v); *)
      let r = pseudo_rem u v in
      (* Format.printf "cont(r) = %a@." D.print (cont r); *)
      if r = [] then
	times (d, 0) v
      else if degree r = 0 then
	[d, 0]
      else
	loop v (pp r)
    in
    loop u v

end


module IntRing = 
struct
  type t = int
  let zero = 0
  let one = 1
  let plus a b = a + b
  let neg a = -a
  let mult a b = a * b
  let eq a b = (a = b)
  let rec gcd x y = if x = 0 then y else gcd (y mod x) x
  let gcd x y = gcd (abs x) (abs y)
  let exact_div x y = x / y
  let div_mod x y = x / y, x mod y
  let print = Format.pp_print_int
end

module P = MakePoly (IntRing)

open Gmp.Z

module GmpRing = struct
  type t = Gmp.Z.t
  let zero = zero
  let one = one
  let neg = neg
  let plus = add
  let mult = mul
  let eq = equal
  let exact_div = divexact
  let div_mod = euclidean_division
  let gcd = gcd
  let print = print
end

module G = MakePoly (GmpRing)

(***
let two = of_int 2
let p1 = P.plus (P.monom one 0) (P.monom one 2) (* 1+X^2 *)
let () = printf "P1 = %a@." P.print p1
let p2 = P.mult p1 p1
let () = printf "P2 = %a@." P.print p2
let () = printf "P2(2) = %a@." print (P.eval p2 two)

let rec random_poly acc = function
  | -1 -> acc
  | n -> random_poly (P.plus (P.monom (of_int (Random.int 1000)) n) acc) (n-1)

let p1 = random_poly P.zero 100
let p2 = random_poly P.zero 100
let () = printf "(P1 + P2)(2) = %a@." print (P.eval (P.mult p1 p2) two)

let () = for i = 1 to 10 do ignore (P.mult p1 p2) done
***)

open Format


(*
#install_printer P.print;;
*)
(* let u = P.from_list [1,8; 1,6; -3,4; -3,3; 8,2; 2,1; -5,0] *)
(* let v = P.from_list [3,6; 5,4; -4,2; -9,1; 21,0] *)
(* let r = P.pseudo_rem u v *)
(* let () = printf "r = %a@." P.print r *)
(* let g = P.gcd u v *)
(* let () = printf "gcd u v = %a@." P.print g *)

(* let u = P.from_list [ 3, 3;  2, 2] *)
(* let v = P.from_list [21, 1; 14, 0] *)
(* let _ = printf "gcd u v = %a@." P.print (P.gcd u v) *)

(*
Local Variables: 
compile-command: "make -C ../.. tests/pasco/poly"
End: 
*)
