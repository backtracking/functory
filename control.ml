(**************************************************************************)
(*                                                                        *)
(*  Functory: a distributed computing library for OCaml                   *)
(*  Copyright (C) 2010- Jean-Christophe Filliatre and Kalyan Krishnamani  *)
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

let debug = ref false

let set_debug b = debug := b

open Format

let dprintf s =
  (if !debug then fprintf else ifprintf) err_formatter s
