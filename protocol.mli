
(** The master/worker protocol. *)

val set_magic_number : int -> unit
  (** if you need to change the default magic number *)

exception BadMagicNumber

exception BadProtocol

(** Note: All IDs are assigned by the master *)

module Master : sig

  type t = 
    | Assign of int * string (* id, task *)
    | Kill of int            (* id *)

  val buf : Buffer.t -> t -> unit

  val get : string -> int -> t * int

end

module Worker : sig

  type t =
    | Ack of int                (* id *)
    | Completed of int * string (* id, result *)
    | Aborted of int            (* id *)

  val buf : Buffer.t -> t -> unit

  val get : string -> int -> t * int

end
