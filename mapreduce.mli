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
module Simple : sig

  val map : f:('a -> 'b) -> 'a list -> 'b list

  val map_local_reduce :
    map:('a -> 'b) -> reduce:('c -> 'b -> 'c) -> 'c -> 'a list -> 'c

  val map_remote_reduce :
    map:('a -> 'b) -> reduce:('c -> 'b -> 'c) -> 'c -> 'a list -> 'c

  val map_reduce_ac :
    map:('a -> 'b) -> reduce:('b -> 'b -> 'b) -> 'b -> 'a list -> 'b

  val map_reduce_a :
    map:('a -> 'b) -> reduce:('b -> 'b -> 'b) -> 'b -> 'a list -> 'b

end

(** Several cores on the same machine *)
module Cores : sig

  val set_number_of_cores : int -> unit

  val map : f:('a -> 'b) -> 'a list -> 'b list
    
  val map_local_reduce :
    map:('a -> 'b) -> reduce:('c -> 'b -> 'c) -> 'c -> 'a list -> 'c

  val map_remote_reduce :
    map:('a -> 'b) -> reduce:('c -> 'b -> 'c) -> 'c -> 'a list -> 'c

  val map_reduce_ac :
    map:('a -> 'b) -> reduce:('b -> 'b -> 'b) -> 'b -> 'a list -> 'b

  val map_reduce_a :
    map:('a -> 'b) -> reduce:('b -> 'b -> 'b) -> 'b -> 'a list -> 'b

end

module Network : sig

  val declare_workers : ?port:int -> ?n:int -> string -> unit
    (** [declare_workers s] declares workers on machine [s];
        the number of workers is [n] and defaults to 1 *)

  (** Polymorphic functions.
      These functions are to be used only when master and worker are
      *exactly* the same binaries. *)

  val map : f:('a -> 'b) -> 'a list -> 'b list

  val map_local_reduce :
    map:('a -> 'b) -> reduce:('c -> 'b -> 'c) -> 'c -> 'a list -> 'c

  val map_remote_reduce :
    map:('a -> 'b) -> reduce:('c -> 'b -> 'c) -> 'c -> 'a list -> 'c

  val map_reduce_ac :
    map:('a -> 'b) -> reduce:('b -> 'b -> 'b) -> 'b -> 'a list -> 'b


  (** Monomorphic functions.
      These functions should be used when master and workers are compiled
      from the same source. *)

  module Str : sig

    val map : f:(string -> string) -> string list -> string list

    val map_local_reduce :
      map:(string -> string) -> reduce:(string -> string -> string) -> 
      string -> string list -> string

    val map_remote_reduce :
      map:(string -> string) -> reduce:(string -> string -> string) ->
      string -> string list -> string

    val map_reduce_ac :
      map:(string -> string) -> reduce:(string -> string -> string) ->
      string -> string list -> string

  end

  module Worker : sig

    val register_computation : string -> (string -> string) -> unit

    val compute : ?stop:bool -> ?port:int -> unit -> string

  end

end


module Control : sig

  val set_debug : bool -> unit
    (** set the debug flag *)

end

