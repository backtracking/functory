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
      (** [map_fold_a map fold acc [x1;...xn]] computes
	  [fold ... (fold (fold acc (map x1)) (map x2)) ... (map xn)]
	  assuming [fold] is an associative
	  operation with neutral element [acc] *)

  val master : 
    f:('a -> 'b) -> 
    handle:('a * 'c -> 'b -> ('a * 'c) list) -> ('a * 'c) list -> unit
      (** [master f handle l] applies function [f] to each first-component
	  of elements in [l]; for each such computation, both the list element
	  and the result are passed to [handle], which returns a list of 
	  new elements to be processed (in an identical manner).
	  The computation stops when there is no more element to be processed.
      *)

  val map_reduce :
    map:('v1 -> ('k2 * 'v2) list) -> 
    reduce:('k2 -> 'v2 list list -> 'v2 list) ->
    'v1 list -> ('k2 * 'v2 list) list
      (** map/reduce a la Google
          uses [Hashtbl.hash] and [Pervasives.compare] on keys of type ['k2] *)

end

(** Several cores on the same machine.
    For documentation, refer to module [Sequential]. *)
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

  val master : 
    f:('a -> 'b) -> 
    handle:('a * 'c -> 'b -> ('a * 'c) list) -> ('a * 'c) list -> unit

(*   val map_reduce : *)
(*     map:('v1 -> ('k2 * 'v2) list) -> *)
(*     reduce:('k2 -> 'v2 list list -> 'v2 list) -> *)
(*     'v1 list -> ('k2 * 'v2 list) list *)

end

(** Network implementation.

    Different network implementations are provided, depending on whether
    - master and workers are the same binary programs (module [Same])
    - master and workers are different programs compiled with the
      same version of Ocaml (module [Poly])
    - master and workers are different programs possibly compiled with 
      different versions of Ocaml (module [Mono])
    

*)

module Network : sig

  val declare_workers : ?port:int -> ?n:int -> string -> unit
    (** [declare_workers s] declares workers on machine [s];
        the number of workers is [n] and defaults to 1.
	Number [n] does not necessarily coincide with the number of 
	available cores	of machine [s]. *)

  (** Same binary executed as master and workers.
      The environment variable WORKER must be set for workers (to be
      distinguished from the master). 

      For documentation, refer to module [Sequential]. *)
  module Same : sig

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

  type worker_type = ?stop:bool -> ?port:int -> unit -> unit
    (** The type of forthcoming worker implementations.
	If set, the [stop] boolean indicates that the worker should 
	stop whenever master disconnects.
	Port number is given by [port]; default value is [51000] and can
	be changed using module [Control] (see below). *)

  (** Polymorphic API (same version of ocaml).

      Contrary to module [Same] above, master and workers are no more
      executing the same code. Submodule [Master] (resp. [Worker]
      provides functions to implement the master (resp. the workers).
      Arguments for functions [master], [map], etc. are thus split between
      these two submodules.

      For documentation, refer to module [Sequential]. *)

  module Poly : sig

    module Master : sig
      val master : 
	handle:('a * 'c -> 'b -> ('a * 'c) list) -> 
	('a * 'c) list -> unit

      val map : 'a list -> 'b list
      val map_local_fold : fold:('c -> 'b -> 'c) -> 'c -> 'a list -> 'c
      val map_remote_fold : 'c -> 'a list -> 'c
      val map_fold_ac : 'b -> 'a list -> 'b
      val map_fold_a : 'b -> 'a list -> 'b
    end

    module Worker : sig
      val compute : ('a -> 'b) -> worker_type
        
      val map : 
	f:('a -> 'b) -> worker_type
      val map_local_fold : 
	map:('a -> 'b) -> worker_type
      val map_remote_fold :
	map:('a -> 'b) -> fold:('c -> 'b -> 'c) -> worker_type
      val map_fold_ac :
	map:('a -> 'b) -> fold:('b -> 'b -> 'b) -> worker_type
      val map_fold_a :
	map:('a -> 'b) -> fold:('b -> 'b -> 'b) -> worker_type
    end
      
  end

  (** Monomorphic API (possibly different versions of ocaml). 
      
      When master and workers are not compiled with the same version of Ocaml,
      only strings can be passed, hence the monomorphic API below.
      It is the responsability of user to encode/decode values.

      For documentation, refer to module [Sequential]. *)

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

end


module Control : sig

  val set_debug : bool -> unit
    (** sets the debug flag *)

  val set_default_port_number : int -> unit
    (** sets the default port number
        (if not called, the default port number is 51000) *)

end

