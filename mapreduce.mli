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

  val map : ('a -> 'b) -> 'a list -> 'b list

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

  val map : ('a -> 'b) -> 'a list -> 'b list
    
  val map_local_reduce :
    map:('a -> 'b) -> reduce:('c -> 'b -> 'c) -> 'c -> 'a list -> 'c

  val map_remote_reduce :
    map:('a -> 'b) -> reduce:('c -> 'b -> 'c) -> 'c -> 'a list -> 'c

end

module Network : sig

  val declare_workers : ?port:int -> ?n:int -> string -> unit
    (** [declare_workers s] declares workers on machine [s];
        the number of workers is [n] and defaults to 1 *)

  val map : (string -> string) -> string list -> string list

  module Worker : sig

    val register_computation : string -> (string -> string) -> unit

    val compute : ?stop:bool -> ?port:int -> unit -> string

  end

end




(****

module Protocol : sig

  module Master : sig
    
    type t = 
      | Assign of int * string * string 
      | Kill of int                     
      | Stop

    val send : Unix.file_descr -> t -> unit
      
    val receive : Unix.file_descr -> t
      
    val print : Format.formatter -> t -> unit
      
  end

  module Worker : sig
    
    type t =
      | Started of int            (* id *)
      | Completed of int * string (* id, result *)
      | Aborted of int            (* id *)
	  
    val send : Unix.file_descr -> t -> unit
      
    val receive : Unix.file_descr -> t
      
    val print : Format.formatter -> t -> unit

  end

end

****)

module Control : sig

  val set_debug : bool -> unit
    (** set the debug flag *)

end

