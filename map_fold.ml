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

type ('a, 'b) map_or_fold =
  | Map of 'a
  | Fold of 'b

let map_fold_wrapper map fold = function
  | Map x -> Map (map x)
  | Fold (x, y) -> Fold (fold x y)

let map_fold_wrapper2 map fold = function
  | Map x -> map x
  | Fold (x, y) -> fold x y

module Make
  (X : sig
     val compute : 
       worker:('a -> 'b) -> 
       master:('a * 'c -> 'b -> ('a * 'c) list) ->
       ('a * 'c) list ->
       unit
   end) :
sig
  
  val map : f:('a -> 'b) -> 'a list -> 'b list
    
  val map_local_fold :
    f:('a -> 'b) -> fold:('c -> 'b -> 'c) -> 'c -> 'a list -> 'c

  val map_remote_fold :
    f:('a -> 'b) -> fold:('c -> 'b -> 'c) -> 'c -> 'a list -> 'c

  val map_fold_ac :
    f:('a -> 'b) -> fold:('b -> 'b -> 'b) -> 'b -> 'a list -> 'b

  val map_fold_a :
    f:('a -> 'b) -> fold:('b -> 'b -> 'b) -> 'b -> 'a list -> 'b

end = struct

  let map ~f l =
    let tasks = let i = ref 0 in List.map (fun x -> incr i; x, !i) l in
    let results = Hashtbl.create 17 in (* index -> 'b *)
    X.compute
      ~worker:f
      ~master:(fun (_,i) r -> Hashtbl.add results i r; [])
      tasks;
    List.map (fun (_,i) -> Hashtbl.find results i) tasks

  let map_local_fold ~(f : 'a -> 'b) ~(fold : 'c -> 'b -> 'c) acc l =
    let acc = ref acc in
    X.compute
      ~worker:f
      ~master:(fun _ r -> acc := fold !acc r; [])
      (List.map (fun x -> x, ()) l);
    !acc 

  let map_remote_fold  ~(f : 'a -> 'b) ~(fold : 'c -> 'b -> 'c) acc l =
    let acc = ref (Some acc) in
    let pending = Stack.create () in
    X.compute
      ~worker:(map_fold_wrapper f fold)
      ~master:(fun _ r -> match r with
		 | Map r -> begin match !acc with
		     | None -> Stack.push r pending; []
		     | Some v -> acc := None; [Fold (v, r), ()]
		   end
		 | Fold r -> 
		     assert (!acc = None);
		     if not (Stack.is_empty pending) then
		       [Fold (r, Stack.pop pending), ()]
		     else begin
		       acc := Some r;
		       []
		     end)
      (List.map (fun x -> Map x, ()) l);
    (* we are done; the accumulator must exist *)
    match !acc with
      | Some r -> r
      | None -> assert false

  let map_fold_ac ~(f : 'a -> 'b) ~(fold : 'b -> 'b -> 'b) acc l =
    let acc = ref (Some acc) in
    X.compute
      ~worker:(map_fold_wrapper2 f fold)
      ~master:(fun _ r -> 
		 match !acc with
		 | None -> 
		     acc := Some r; []
		 | Some v -> 
		     acc := None; 
		     [Fold (v, r), ()])
      (List.map (fun x -> Map x, ()) l);
    (* we are done; the accumulator must exist *)
    match !acc with
      | Some r -> r
      | None -> assert false

  let map_fold_a ~(f : 'a -> 'b) ~(fold : 'b -> 'b -> 'b) acc l =
    let tasks = 
      let i = ref 0 in 
      List.map (fun x -> incr i; Map x, (!i, !i)) l 
    in
    (* results maps i and j to (i,j,r) for each completed reduction of
       the interval i..j with result r *)
    let results = Hashtbl.create 17 in 
    let merge i j r = 
      if Hashtbl.mem results (i-1) then begin
	let l, h, x = Hashtbl.find results (i-1) in
	assert (h = i-1);
	Hashtbl.remove results l; 
	Hashtbl.remove results h;
	[Fold (x, r), (l, j)]
      end else if Hashtbl.mem results (j+1) then begin
	let l, h, x = Hashtbl.find results (j+1) in
	assert (l = j+1);
	Hashtbl.remove results h; 
	Hashtbl.remove results l;
	[Fold (r, x), (i, h)]
      end else begin
	Hashtbl.add results i (i,j,r);
	Hashtbl.add results j (i,j,r);
	[]
      end
    in
    X.compute 
      ~worker:(map_fold_wrapper2 f fold)
      ~master:(fun x r -> match x with
		 | Map _, (i, _) -> merge i i r
		 | Fold _, (i, j) -> merge i j r)
      tasks;
    (* we are done; results must contain 2 mappings only, for 1 and n *)
    try let _,_,r = Hashtbl.find results 1 in r with Not_found -> acc

end
