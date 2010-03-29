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

(* coding *)

let buf_int8 buf i =
  Buffer.add_char buf (char_of_int (i land 255))

let buf_int31 buf i = 
  if i < 0 then invalid_arg "buf_int31: negative argument";
  buf_int8 buf i;
  buf_int8 buf (i lsr 8);
  buf_int8 buf (i lsr 16); 
  buf_int8 buf (i lsr 24)

let buf_string buf s =
  buf_int31 buf (String.length s);
  Buffer.add_string buf s

let buf_string_list buf l =
  buf_int31 buf (List.length l);
  List.iter (buf_string buf) l

(* decoding *)

exception IncompleteMessage

let get_uint8 s pos = 
  int_of_char s.[pos], pos+1

let get_int31 s pos =
  let c1, pos = get_uint8 s pos in
  let c2, pos = get_uint8 s pos in
  let c3, pos = get_uint8 s pos in
  let c4, pos = get_uint8 s pos in
  let x = c1 lor (c2 lsl 8) lor (c3 lsl 16) lor (c4 lsl 24) in
  x, pos

let get_string s pos =
  let len, pos = get_int31 s pos in
  String.sub s pos len, pos+len

let get_string_list s pos =
  let len, pos = get_int31 s pos in
  let rec read acc pos n = 
    if n = 0 then
      List.rev acc, pos
    else 
      let s, pos = get_string s pos in
      read (s :: acc) pos (n - 1)
  in
  read [] pos len

