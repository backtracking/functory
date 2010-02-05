
open Format
open Binary


(* magic number *)

let magic_number = ref 0x19712010
let set_magic_number n = magic_number := n

exception BadMagicNumber

let check_magic_number s pos = 
  let m, pos = get_int31 s pos in
  if m <> !magic_number then raise BadMagicNumber;
  pos

(* generic send / receive *)

let write_buffer = Buffer.create 1024

let generic_send buf fd t =
  Buffer.clear write_buffer;
  buf write_buffer t;
  let s = Buffer.contents write_buffer in
  let len = String.length s in
  Buffer.clear write_buffer;
  Binary.buf_int31 write_buffer len;
  let slen = Buffer.contents write_buffer in
  assert (Unix.write fd slen 0 4 = 4);
  assert (Unix.write fd s 0 len = len)

let read_buffer = String.create 1024

let generic_receive get fd = 
  assert (Unix.read fd read_buffer 0 4 = 4);
  let len, _ = get_int31 read_buffer 0 in
  let s = if len <= 1024 then read_buffer else String.create len in
  assert (Unix.read fd s 0 len = len);
  let t, _ = get s 0 in
  t

(* master *)

exception BadProtocol

module Master = struct

  type t = 
    | Assign of int * string (* id, task *)
    | Kill of int            (* id *)

  let print fmt = function
    | Assign (id, s) ->
	fprintf fmt "assign %d s=%S" id s
    | Kill id ->
	fprintf fmt "kill %d" id

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

  let send = generic_send buf

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

  let receive = generic_receive get

end

module Worker = struct

  type t =
    | Started of int            (* id *)
    | Completed of int * string (* id, result *)
    | Aborted of int            (* id *)

  let print fmt = function
    | Started id -> 
	fprintf fmt "started %d" id
    | Completed (id, s) ->
	fprintf fmt "completed %d s=%S" id s
    | Aborted id ->
	fprintf fmt "aborted %d" id

  let buf b = function
    | Started id -> 
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
	
  let send = generic_send buf

  let get s pos =
    let pos = check_magic_number s pos in
    let c, pos = get_uint8 s pos in
    match c with
      | 3 (* = ack *) ->
	  let id, pos = get_int31 s pos in
	  Started id, pos
      | 4 (* = completed *) ->
	  let id, pos = get_int31 s pos in
	  let s, pos = get_string s pos in
	  Completed (id, s), pos
      | 5 (* = aborted *) ->
	  let id, pos = get_int31 s pos in
	  Aborted id, pos
      | _ ->
	  raise BadProtocol

  let receive = generic_receive get

end
