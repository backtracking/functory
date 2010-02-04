
(* this code is from *)

(**************************************************************************)
(*                                                                        *)
(*  Ocamlviz --- real-time profiling tools for Objective Caml             *)
(*  Copyright (C) by INRIA - CNRS - Universite Paris Sud                  *)
(*  Authors: Julien Robert                                                *)
(*           Guillaume Von Tokarski                                       *)
(*           Sylvain Conchon                                              *)
(*           Jean-Christophe Filliatre                                    *)
(*           Fabrice Le Fessant                                           *)
(*  GNU Library General Public License version 2                          *)
(*  See file LICENSE for details                                          *)
(*                                                                        *)
(**************************************************************************)

(** Low-level functions for the binary protocol. *)

(** Coding functions *)

val buf_int8 :  Buffer.t -> int -> unit
(** [buf_int8 b i] encodes a 8-bits integer [i] in the buffer [b]. *)

val buf_int31 :  Buffer.t -> int -> unit
  (** [buf_int31 b i] encodes a 31-bits integer [i] in the buffer [b].
      [i] must be positive. On a 64-bits architecture, it is truncated to 
      31 bits. *)

val buf_string :  Buffer.t -> string -> unit
  (** [buf_string b s] encodes a string in the buffer [b].
      The string length must fit into a signed 31-bits integer. *)

exception IncompleteMessage

(** Decoding functions *)

val get_uint8 : string -> int -> int * int
  (** [get_uint8 s pos] decodes a byte in [s] at the position [pos],
      along with the new position in [s] (which is [pos+1] here). *)

val get_int31 : string -> int -> int * int
  (** [get_int31 s pos] decodes four bytes in [s] at the position [pos] 
      and return a 31 bits integer along with the new position in [s]. *)

val get_string : string -> int -> string * int
  (** [get_string s pos] decodes a string in [s] at the position [pos] 
      and returns it along with the new position in [s]. *)

