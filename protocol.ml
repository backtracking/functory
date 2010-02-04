
open Binary


(* magic number *)

let magic_number = ref 0x19712010
let set_magic_number n = magic_number := n

exception BadMagicNumber

let check_magic_number s pos = 
  let m, pos = get_int31 s pos in
  if m <> !magic_number then raise BadMagicNumber;
  pos

(* master *)

exception BadProtocol

module Master = struct

  type t = 
    | Assign of int * string (* id, task *)
    | Kill of int            (* id *)

  let buf b = function
    | Assign (id, s) -> 
	buf_int31 b !magic_number;
	buf_int8 b 1; (* 1 = assign *)
	buf_int31 b id;
	buf_string b s
    | Kill id ->
	buf_int31 b !magic_number;
	buf_int8 b 2; (* 2 = kill *)
	buf_int31 b id

  let get s pos =
    let pos = check_magic_number s pos in
    let c, pos = get_uint8 s pos in
    match c with
      | 1 (* assign *) -> 
	  let id, pos = get_int31 s pos in
	  let s, pos = get_string s pos in
	  Assign (id, s), pos
      | 2 (* kill *) ->
	  let id, pos = get_int31 s pos in
	  Kill id, pos
      | _ ->
	  raise BadProtocol

end

module Worker = struct

  type t =
    | Ack of int                (* id *)
    | Completed of int * string (* id, result *)
    | Aborted of int            (* id *)

  let buf b = function
    | Ack id -> 
	buf_int31 b !magic_number;
	buf_int8 b 3; (* 3 = ack *)
	buf_int31 b id
    | Completed (id, s) ->
	buf_int31 b !magic_number;
	buf_int8 b 4; (* 4 = completed *)
	buf_int31 b id;
	buf_string b s
    | Aborted id ->
	buf_int31 b !magic_number;
	buf_int8 b 5; (* 5 = aborted *)
	buf_int31 b id
	
  let get s pos =
    let pos = check_magic_number s pos in
    let c, pos = get_uint8 s pos in
    match c with
      | 3 (* = ack *) ->
	  let id, pos = get_int31 s pos in
	  Ack id, pos
      | 4 (* = completed *) ->
	  let id, pos = get_int31 s pos in
	  let s, pos = get_string s pos in
	  Completed (id, s), pos
      | 5 (* = aborted *) ->
	  let id, pos = get_int31 s pos in
	  Aborted id, pos
      | _ ->
	  raise BadProtocol

end
