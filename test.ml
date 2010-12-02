(**************************************************************************)
(*                                                                        *)
(*  Functory: a distributed computing library for Ocaml                   *)
(*  Copyright (C) 2010 Jean-Christophe Filliatre and Kalyan Krishnamani   *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2.1, with the special exception on linking            *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

open Format

let () = 
  Arg.parse
    ["-d", Arg.Unit (fun () -> Functory.Control.set_debug true), 
     "sets the debug flag";]
    (fun _ -> ())
    "test: usage:"

module type MF = sig
  type t
  val map : f:(t -> t) -> t list -> t list
  val map_local_fold : 
    f:(t -> t) -> fold:(t -> t -> t) -> t -> t list -> t
  val map_remote_fold :
    f:(t -> t) -> fold:(t -> t -> t) -> t -> t list -> t
  val map_fold_ac :
    f:(t -> t) -> fold:(t -> t -> t) -> t -> t list -> t
  val map_fold_a :
    f:(t -> t) -> fold:(t -> t -> t) -> t -> t list -> t
  val compute :
    worker:('a -> 'b) -> 
    master:('a * 'c -> 'b -> ('a * 'c) list) -> ('a * 'c) list -> unit
end

module TestInt(X : MF with type t = int) = struct
  open X

  let f x = x+1

  let fold = (+)

  let () =
    let l = [1;2;3;4;5] and r = 20 in
    printf "  map@.";
    assert (map f l = [2;3;4;5;6]);
    printf "  map_local_fold@.";
    assert (map_local_fold ~f ~fold 0 l = r);
    printf "  map_remote_fold@.";
    assert (map_remote_fold ~f ~fold 0 l = r);
    printf "  map_fold_ac@.";
    assert (map_fold_ac ~f ~fold 0 l = r);
    printf "  map_fold_a@.";
    assert (map_fold_a ~f ~fold 0 l = r);
    printf "  master@.";
    assert (
      let res = ref 0 in
      compute 
	~worker:f 
	~master:(fun (x,y) z -> 
		   assert (z = x+1 && x = y); 
		   res := !res + z;
		   if x = 3 then [4,4] else []) 
	[1,1; 2,2; 3,3];
      !res = 14);
    ()

end

module TestString(X : MF with type t = string) = struct
  open X

  let f s = s ^ "."
    
  let fold = (^)
    
  let () =
    let l = ["a"; "bb"; "ccc"; "dddd"] in
    assert (map f l = ["a."; "bb."; "ccc."; "dddd."]);
    let check r = 
      String.length r = 14 &&
      List.for_all 
      (fun x -> 
	 let i = String.index r x.[0] in 
	 let n = String.length x in 
	 String.sub r i n = x && r.[i + n] = '.')
      l
    in
    printf "  map_local_fold@.";
    assert (check (map_local_fold ~f ~fold "" l));
    printf "  map_remote_fold@.";
    assert (check (map_remote_fold ~f ~fold "" l));
    printf "  map_fold_ac@.";
    assert (check (map_fold_ac ~f ~fold "" l));
    printf "  map_fold_a@.";
    assert (map_fold_a ~f ~fold "" l = "a.bb.ccc.dddd.");
    ()
end

(****
module TestMR(X : 
  sig 
    val map_reduce :
      map:('v1 -> ('k2 * 'v2) list) -> 
      reduce:('k2 -> 'v2 list list -> 'v2 list) ->
      'v1 list -> ('k2 * 'v2 list) list
  end) = struct

  let text = "En l'année 1872, la maison portant le numéro 7 de Saville-row, Burlington Gardens -- maison dans laquelle Sheridan mourut en 1814 --, était habitée par Phileas Fogg, esq., l'un des membres les plus singuliers et les plus remarqués du Reform-Club de Londres, bien qu'il semblât prendre à tâche de ne rien faire qui pût attirer l'attention.

A l'un des plus grands orateurs qui honorent l'Angleterre, succédait donc ce Phileas Fogg, personnage énigmatique, dont on ne savait rien, sinon que c'était un fort galant homme et l'un des plus beaux gentlemen de la haute société anglaise."  

  let is_char j = match text.[j] with
    | ' ' | ',' | '.' | '\'' | '-' -> false
    | _ -> true

  let words =
    let n = String.length text in
    let rec split acc i =
      let rec next j = 
	if j = n || not (is_char j) then 
	  j, String.sub text i (j - i)
	else 
	  next (j + 1)
      in
      let rec adv i = if i < n && not (is_char i) then adv (i+1) else i in
      if i = n then acc else let i, s = next i in split (s :: acc) (adv i)
    in
    split [] 0

  let chunk_size = List.length words / 3

  let rec create_chunk acc i = function
    | [] -> acc, []
    | l when i = 0 -> acc, l
    | x :: l -> create_chunk (x :: acc) (i - 1) l

  let rec split acc l =
    if l = [] then acc 
    else let c, l = create_chunk [] chunk_size l in split (c :: acc) l
					     
  let chunks = split [] words

  let () = printf "  map_reduce@."

  let merge l1 l2 =
    let rec merge acc = function
      | [], l | l, [] -> 
	  List.rev_append acc l
      | x1 :: r1, (x2 :: _ as l2) when x1 <= x2 -> 
	  merge (x1 :: acc) (r1, l2)
      | l1, x2 :: r2 ->
	  merge (x2 :: acc) (l1, r2)
    in
    merge [] (l1, l2)

  let wc =
    X.map_reduce 
      ~map:(fun w -> List.map (fun x -> x,1) w)
      ~reduce:(fun _ l -> [List.fold_left (+) 0 (List.flatten l)]) 
      chunks

  let () = 
    assert (List.assoc "l" wc = [6]);
    assert (List.assoc "Fogg" wc = [2])

end
***)

let () = printf "Sequential@."
module TestIntSeq = 
  TestInt(struct type t = int include Functory.Sequential end)
module TestStringSeq = 
  TestString(struct type t = string include Functory.Sequential end)
(* module TestMRSeq =  *)
(*   TestMR(Functory.Sequential) *)

let () = printf "Cores@."
let () = Functory.Cores.set_number_of_cores 2
module TestIntCores = 
  TestInt(struct type t = int include Functory.Cores end)
module TestStringCores = 
  TestString(struct type t = string include Functory.Cores end)
(* module TestMRCores =  *)
(*   TestMR(Functory.Cores) *)

let () = printf "Network@."
let () = Functory.Network.declare_workers ~n:2 "localhost"
module TestIntNetwork =
  TestInt(struct type t = int include Functory.Network.Same end)
(* module TestStringNetwork = *)
(*   TestString(struct type t = string include Functory.Network.Same end) *)
(* module TestStringNetworkStr =  *)
(*   TestString(struct type t = string include Functory.Network.Str end) *)



(***********

let rec compute x = if x <= 1 then 1 else x * compute (x-1)

(* let n = map_fold_a ~map:compute ~fold:(+) 0 [1;2;3;4;5;6;7;8;9] *)

(* let () = printf "%d@." n *)

let f x = sprintf ".%d." x

let s = map_fold_a ~map:f ~fold:(^) "" [1;2;3;4;5;6;7]

let () = printf "%s@." s; exit 0



let l = map compute [1;2;3;4]

let () = List.iter (fun s -> printf "%d@." s) l; printf "---@."

let l = map compute l

let () = List.iter (fun s -> printf "%d@." s) l; printf "---@."


let compute s = 
  let rec fib n = if n <= 1 then 1 else fib (n-1) + fib (n-2) in
  string_of_int (fib (int_of_string s))

let l = map compute ["10"; "20"; "15"] 

let () = List.iter (fun s -> printf "%s@." s) l; printf "---@."

let l = map compute ["10"; "20"; "15"] 

let () = List.iter (fun s -> printf "%s@." s) l; printf "---@."

let l = map compute ["5"; "6"; "7"; "8"; "9"; "10"; "11"; "12"; "13";
		     "20"; "30"; "40"; ] 

let () = List.iter (fun s -> printf "%s@." s) l
************)
