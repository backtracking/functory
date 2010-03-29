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

(** The master/worker protocol. *)

val set_magic_number : int -> unit
  (** if you need to change the default magic number *)

exception BadMagicNumber

exception BadProtocol

(** Note: All IDs are assigned by the master *)

module Master : sig

  type t = 
    | Assign of int * string * string (* id, function, argument *)
    | Kill of int                     (* id *)
    | Stop of string
    | Ping

  val send : Unix.file_descr -> t -> unit

  val receive : Unix.file_descr -> t

  val print : Format.formatter -> t -> unit

end

module Worker : sig

  type t =
    | Pong
    | Completed of int * string (* id, result *)
    | Aborted of int            (* id *)

  val send : Unix.file_descr -> t -> unit

  val receive : Unix.file_descr -> t

  val print : Format.formatter -> t -> unit

end
