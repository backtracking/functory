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

  val map_reduce :
    map:('a -> 'b list) ->
    reduce:('c -> 'b -> 'c) -> 
    'c ->
    'a list -> 
    'c

  val map : ('a -> 'b) -> 'a list -> 'b list

end

(** Several cores on the same machine *)
module Cores : sig

  val map : ('a -> 'b) -> 'a list -> 'b list

end
