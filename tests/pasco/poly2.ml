open Format
open Gmp

(* Some General Comments: *)
(* In the following c_i denotes coefficients and x_i denotes the
   variable of the polynomial. And I adopt 'add' and 'equal' for addition
   and equality-testing instead of 'plus' and 'eq' as I find it more
   natural. 'D' stands for domain and so the POLYNOMIAL module can be
   instantiated with the domain of integers or other interesting domains
   using the functor MakePoly. *)

module type RING  = sig
  type t 
  val zero : t
  val one : t
  val add : t -> t -> t
  val mult : t -> t -> t
  val equal : t -> t -> bool
  val gcd : t -> t -> t
  val print : formatter -> t -> unit
end
  
module type POLYNOMIAL = sig
  type c 				(* type of coefficient *)
  type t				(* type of the polynomial itself *)
  val zero : t
  val one : t
  val monom : c -> int -> t 		(* coeff * power (2, 2) for 2.x^2 *)
  val add : t -> t -> t
  val equal : t -> t -> bool
  val mult : t -> t -> t
  val eval : t -> c -> c
  val cont : t -> c
  (* val gcd : t -> t -> t *)
  val from_list : (c * int) list -> t
  val print : formatter -> t -> unit
end

module MakePoly (D : RING)
  : POLYNOMIAL with type c = D.t = 
struct
  type c = D.t
  type monom = (c * int)
  type t = monom list
  let zero = []
  let one = [D.one, 0]

  (* helper functions *)

  (* let coeff_gcd x y =  *)
  (*     if (x < 0) or (y < 0) then *)
  (* 	(0 - 1) * (gcd (abs x) (abs y)) *)
  (*     else *)
  (* 	gcd x y *)
	  
  (* coeff list *)
  let cont p = 
    let coeff_list p = 
      List.map (fun x -> fst x) p 
    in
      List.fold_left D.gcd (List.nth (coeff_list p) 1) (coeff_list p)
      
  (* pp *)
  (* let pp p = List.map (fun x -> x / cont p) p *)
    
	
  (* Monom *)
  let monom c x = 
    if (x < 0) then
      failwith "power cannot be negative"
    else if D.equal c D.zero then
      []
    else [(c, x)]
      
  (* Equality-testing *)
  let rec equal p1 p2 = 
    match p1, p2 with
      | [], [] -> true
      | (c1, x1) :: q1, (c2, x2) :: q2 ->
	  (x1 = x2) && (D.equal c1 c2) && (equal q1 q2)
      | _ -> false

  (* Addition of polynomials *)
  let rec add p1 p2 = 
    match p1, p2 with
      | ((c1, x1) as m1) :: q1, ((c2, x2) as m2) :: q2 ->
	  if (x1 < x2) then
	    m1 :: (add q1 p2)
	  else if (x1 = x2) then 
	    let c = D.add c1 c2 in 
	      if D.equal c D.zero then add q1 q2
	      else (c, x1) :: (add q1 q2)
	  else
	      m2 :: (add p1 q2)
      | _, [] -> p1
      | [], _ -> p2

  (* Helper function for multiplication *)
  let rec times (c1, x1 as m) p = 
    match p with
      | [] -> []
      | (c2, x2) :: q -> 
	  let k = D.mult c1 c2 in
	    if D.equal k D.zero then times m q
	    else (k, x1 + x2) :: (times m q)

  (* Multiplication *)
  let mult p2 = 
    List.fold_left (fun p1 m -> add p1 (times m p2)) zero

  (* Power : Given c, x, computes c^x *)
  let rec pow c x = match x with
    | 0 -> D.one
    | 1 -> c
    | x ->
	let l = pow c (x/2) in 
	let l2 = D.mult l l in 
	  if x mod 2 = 0 then l2 else D.mult c l2

  (* Evaluation of Polynomial - Horner's Schema *)
  let eval p v = 
    let rec eval_loop acc = function 
      | [] -> acc
      | [c, x] -> D.mult (pow v x) (D.add c acc)
      | (c1, x1) :: ((c2, x2) :: _ as q) -> 
	  eval_loop (D.mult (pow v (x1 - x2)) (D.add c1 acc)) q
    in
      eval_loop D.zero (List.rev p)

  (* Printing polynomials *)
  let print fmt p = 
    fprintf fmt "( ";
    let b = List.fold_left 
      (fun acc (c, x) ->
	 if acc then fprintf fmt " + ";
	 fprintf fmt "%a x^%d" D.print c x;
	 true
      ) false p in 
      if (not b) then (D.print fmt D.zero);
      fprintf fmt " )\n"
end

module ZRing = 
struct
  type t = int
  let zero = 0
  let one = 1
  let add a b = a + b
  let mult a b = a * b 
  let equal a b = (a = b)
  let print = pp_print_int
  let rec gcd x y = 
    if x = 0 then y
    else if y = 0 then x
    else if x > y then gcd y (x mod y)
    else gcd x (y mod x)
end

(* tests *)
module P = MakePoly (ZRing)
let p1 = P.add (P.monom 4 0) (P.add (P.monom 2 3) (P.monom 1 1))
let () = printf "%a@." P.print p1
let () = printf "%d@." (P.eval p1 2)

let u = P.add (P.monom (-26) 2) (P.monom 39 0)

(*
Local Variables: 
compile-command: "make -C ../.. tests/pasco/poly2"
End: 
*)
