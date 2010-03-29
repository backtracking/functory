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

(** Sequential implementation *)

let map ~f l = List.map f l

let map_local_fold ~map ~fold acc l =
  List.fold_left (fun acc x -> fold acc (map x)) acc l

let map_remote_fold = map_local_fold

let map_fold_ac = map_local_fold

let map_fold_a = map_local_fold

let map_reduce ~map ~reduce l =
  let h = Hashtbl.create 5003 in
  let add (k2, v2) =
    try Hashtbl.replace h k2 ([v2] :: Hashtbl.find h k2)
    with Not_found -> Hashtbl.add h k2 [[v2]]
  in
  List.iter (fun v1 -> List.iter add (map v1)) l;
  Hashtbl.fold (fun k2 v2l res -> (k2, reduce k2 v2l) :: res) h []


let compute ~worker ~master l =
  let pending = Stack.create () in
  let add l = List.iter (fun t -> Stack.push t pending) l in
  add l;
  while not (Stack.is_empty pending) do
    let a,_ as t = Stack.pop pending in
    let l = master t (worker a) in
    add l
  done

