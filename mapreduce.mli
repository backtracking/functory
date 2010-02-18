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

end

module Network : sig

  val declare_workers : ?port:int -> ?n:int -> string -> unit
    (** [declare_workers s] declares workers on machine [s];
        the number of workers is [n] and defaults to 1 *)

  (** Polymorphic functions.
      These functions are to be used only when master and worker are
      *exactly* the same binaries. *)

  val map : f:('a -> 'b) -> 'a list -> 'b list

  val map_local_fold :
    map:('a -> 'b) -> fold:('c -> 'b -> 'c) -> 'c -> 'a list -> 'c

  val map_remote_fold :
    map:('a -> 'b) -> fold:('c -> 'b -> 'c) -> 'c -> 'a list -> 'c

  val map_fold_ac :
    map:('a -> 'b) -> fold:('b -> 'b -> 'b) -> 'b -> 'a list -> 'b

  val map_fold_a :
    map:('a -> 'b) -> fold:('b -> 'b -> 'b) -> 'b -> 'a list -> 'b

  (** Monomorphic functions.
      These functions should be used when master and workers are compiled
      from the same source. *)

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


  (** General case: master and workers are developped independently *)

  module Master : sig

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

   end

  module Worker : sig

    val register_computation : string -> (string -> string) -> unit
      (** [register_computation n f] registers function [f] with name [n] *)

    val register_computation2 : string -> (string -> string -> string) -> unit
      (** [register_computation2 n f] registers the two arguments 
	  function [f] with name [n] *)

    val compute : ?stop:bool -> ?port:int -> unit -> string
      (** [compute ()] starts the worker loop, listening on port [port]
	  (default is 51000). 

	  When [stop] is set to [true], which is the defautl,
	  the worker will stop whenever it receives
	  the Stop command from the master and will then return the result
	  provided by the master.
	  
	  Whenever [stop] is set to [false], [compute] never returns
	  and accept several parallel connections from masters. *)

  end

end


module Control : sig

  val set_debug : bool -> unit
    (** set the debug flag *)

end

