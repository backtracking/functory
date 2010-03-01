(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) Jean-Christophe Filliatre and Kalyan Krishnamani        *)
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

(** Map/Reduce Paradigm *)

(** The sequential implementation (to be used as a reference) *)
module Sequential : sig

  val map : f:('a -> 'b) -> 'a list -> 'b list
      (** same result as [List.map] *)

  val map_local_fold :
    map:('a -> 'b) -> fold:('c -> 'b -> 'c) -> 'c -> 'a list -> 'c
      (** [map_local_fold map fold acc l] computes
	  [fold ... (fold (fold acc (map x1)) (map x2)) ... (map xn)]
	  for some permutation [x1,x2,...,xn] of [l] *)

  val map_remote_fold :
    map:('a -> 'b) -> fold:('c -> 'b -> 'c) -> 'c -> 'a list -> 'c
      (** same result *)

  val map_fold_ac :
    map:('a -> 'b) -> fold:('b -> 'b -> 'b) -> 'b -> 'a list -> 'b
      (** same result, assuming [fold] is an associative and commutative
	  operation with neutral element [acc] *)
    
  val map_fold_a :
    map:('a -> 'b) -> fold:('b -> 'b -> 'b) -> 'b -> 'a list -> 'b
      (** same result, assuming [fold] is an associative
	  operation with neutral element [acc] *)

  val map_reduce :
    map:('v1 -> ('k2 * 'v2) list) -> 
    reduce:('k2 -> 'v2 list list -> 'v2 list) ->
    'v1 list -> ('k2 * 'v2 list) list
      (** map/reduce a la Google
          uses [Hashtbl.hash] and [Pervasives.compare] on keys of type ['k2] *)

end

(** Several cores on the same machine *)
module Cores : sig

  val set_number_of_cores : int -> unit

  val map : f:('a -> 'b) -> 'a list -> 'b list
    
  val map_local_fold :
    map:('a -> 'b) -> fold:('c -> 'b -> 'c) -> 'c -> 'a list -> 'c

  val map_remote_fold :
    map:('a -> 'b) -> fold:('c -> 'b -> 'c) -> 'c -> 'a list -> 'c

  val map_fold_ac :
    map:('a -> 'b) -> fold:('b -> 'b -> 'b) -> 'b -> 'a list -> 'b

  val map_fold_a :
    map:('a -> 'b) -> fold:('b -> 'b -> 'b) -> 'b -> 'a list -> 'b

(*   val map_reduce : *)
(*     map:('v1 -> ('k2 * 'v2) list) -> *)
(*     reduce:('k2 -> 'v2 list list -> 'v2 list) -> *)
(*     'v1 list -> ('k2 * 'v2 list) list *)

  val master : 
    f:('a -> 'b) -> 
    handle:('a * 'c -> 'b -> ('a * 'c) list) -> ('a * 'c) list -> unit

end

(** Same binary executed as master and workers *)
module Network1 : sig

  val map : f:('a -> 'b) -> 'a list -> 'b list

  val map_local_fold :
    map:('a -> 'b) -> fold:('c -> 'b -> 'c) -> 'c -> 'a list -> 'c

  val map_remote_fold :
    map:('a -> 'b) -> fold:('c -> 'b -> 'c) -> 'c -> 'a list -> 'c

  val map_fold_ac :
    map:('a -> 'b) -> fold:('b -> 'b -> 'b) -> 'b -> 'a list -> 'b

  val map_fold_a :
    map:('a -> 'b) -> fold:('b -> 'b -> 'b) -> 'b -> 'a list -> 'b

  val master : 
    f:('a -> 'b) -> 
    handle:('a * 'c -> 'b -> ('a * 'c) list) -> ('a * 'c) list -> unit

end

module Network : sig

  val declare_workers : ?port:int -> ?n:int -> string -> unit
    (** [declare_workers s] declares workers on machine [s];
        the number of workers is [n] and defaults to 1 *)

  (** Polymorphic functions. (same version of ocaml) *)

  type worker_type = ?port:int -> unit -> unit

  module Poly : sig

    module Master : sig
      val master : 
	handle:('a * 'c -> 'b -> ('a * 'c) list) -> 
	('a * 'c) list -> unit

      val map : 'a list -> 'b list
    end

    module Worker : sig
      val compute : ('a -> 'b) -> worker_type
        
      val map : f:('a -> 'b) -> worker_type
    end
      
  end

  (** Monomorphic functions. (possibly different versions of ocaml) *)

  module Mono : sig

    module Master : sig
      val master : 
	handle:(string * 'c -> string -> (string * 'c) list) -> 
	(string * 'c) list -> unit
    end

    module Worker : sig
      val compute : (string -> string) -> worker_type
    end

  end

  (***

  module Str : sig

    val map : f:(string -> string) -> string list -> string list

    val map_local_fold :
      map:(string -> string) -> fold:(string -> string -> string) -> 
      string -> string list -> string

    val map_remote_fold :
      map:(string -> string) -> fold:(string -> string -> string) ->
      string -> string list -> string

    val map_fold_ac :
      map:(string -> string) -> fold:(string -> string -> string) ->
      string -> string list -> string

    val map_fold_a :
      map:(string -> string) -> fold:(string -> string -> string) ->
      string -> string list -> string

  end

  ***)


    (***
    val map : string list -> string list
      (** [map l] is returning [List.map f l] where
	  [f : string -> string] is a function registered under name "f" *)

    val map_local_fold :
      fold:(string -> string -> string) ->
      string -> string list -> string
      (** [map_local_fold fold acc l] is returning 
	  [map_local_fold map fold acc l] where [map : string -> string]
	  is a function registered under name "map" *)

    val map_remote_fold : string -> string list -> string
      (** [map_remote_fold acc l] is returning 
	  [map_remote_fold map fold acc l] where [map : string -> string]
	  and [fold : string -> string -> string]
	  are functions respectively registered under names "map" and
          "fold" *)

    val map_fold_ac : string -> string list -> string
      (** workers should provide a "map" function and a "fold" function *)

    val map_fold_a : string -> string list -> string
      (** workers should provide a "map" function and a "fold" function *)
    ***) 



end


module Control : sig

  val set_debug : bool -> unit
    (** set the debug flag *)

end

