
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
