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

(** {b Distributed computing library} *)

(** The sequential implementation (to be used as a reference) *)
module Sequential : sig

  (** {2 Generic API} *)

  val compute : 
    worker:('a -> 'b) -> 
    master:('a * 'c -> 'b -> ('a * 'c) list) -> ('a * 'c) list -> unit
      (** [master f handle l] applies function [f] to each first-component
	  of elements in [l]; for each such computation, both the list element
	  and the result are passed to [handle], which returns a list of 
	  new elements to be processed (in an identical manner).
	  The computation stops when there is no more element to be processed.
      *)

  (** {2 Derived API}

      The following functions are provided for convenience; 
      they can be derived from the generic function above. *)

  val map : f:('a -> 'b) -> 'a list -> 'b list
      (** same result as [List.map] *)

  val map_local_fold :
    f:('a -> 'b) -> fold:('c -> 'b -> 'c) -> 'c -> 'a list -> 'c
      (** [map_local_fold f fold acc l] computes
	  [fold ... (fold (fold acc (f x1)) (f x2)) ... (f xn)]
	  for some permutation [x1,x2,...,xn] of [l] *)

  val map_remote_fold :
    f:('a -> 'b) -> fold:('c -> 'b -> 'c) -> 'c -> 'a list -> 'c
      (** same specification as above *)

  val map_fold_ac :
    f:('a -> 'b) -> fold:('b -> 'b -> 'b) -> 'b -> 'a list -> 'b
    (** same specification, assuming [fold] is an associative and
	commutative operation; the third argument should be a
	neutral element for [fold] *)
    
  val map_fold_a :
    f:('a -> 'b) -> fold:('b -> 'b -> 'b) -> 'b -> 'a list -> 'b
    (** [map_fold_a f fold acc [x1;...xn]] computes
	[fold ... (fold (fold acc (f x1)) (f x2)) ... (f xn)]
	assuming [fold] is an associative
	operation with neutral element [acc] *)

end

(** Several cores on the same machine. *)
module Cores : sig

  val set_number_of_cores : int -> unit
    (** [set_number_of_cores n] indicates that [n] computations can be 
	done in parallel. It is typically less or equal to the number of
	actual cores on the machine, though it is not mandatory.
        Setting [n] to 1 is equivalent to a sequential execution (though the 
	order in which tasks are performed may differ). *)

  (** {2 Generic API}

      For documentation, refer to module {!Sequential}. *)

  val compute : 
    worker:('a -> 'b) -> 
    master:('a * 'c -> 'b -> ('a * 'c) list) -> ('a * 'c) list -> unit

  (** {2 Derived API}

      For documentation, refer to module {!Sequential}. *)

  val map : f:('a -> 'b) -> 'a list -> 'b list
    
  val map_local_fold :
    f:('a -> 'b) -> fold:('c -> 'b -> 'c) -> 'c -> 'a list -> 'c

  val map_remote_fold :
    f:('a -> 'b) -> fold:('c -> 'b -> 'c) -> 'c -> 'a list -> 'c

  val map_fold_ac :
    f:('a -> 'b) -> fold:('b -> 'b -> 'b) -> 'b -> 'a list -> 'b

  val map_fold_a :
    f:('a -> 'b) -> fold:('b -> 'b -> 'b) -> 'b -> 'a list -> 'b

end

(** Network implementation.

    Different network implementations are provided, depending on whether
    - master and workers are the same binary programs (module {!Same})
    - master and workers are different programs compiled with the
      same version of Ocaml (module {!Poly})
    - master and workers are different programs possibly compiled with 
      different versions of Ocaml (module {!Mono})
*)
module Network : sig

  (** {2 Setup} *)

  type worker

  val create_worker : ?port:int -> string -> worker
    (** [create_worker s] creates a new worker located on machine [s].
        Raises [Invalid_argument] if [s] is not a valid machine. *)

  val declare_workers : ?port:int -> ?n:int -> string -> unit
    (** [declare_workers s] declares [n] workers on machine [s]
        (when [n] is not given, it defaults to 1).
	Number [n] does not necessarily coincide with the number of 
	available cores	of machine [s]. 
	[s] could be a machine hostname or an IP number.
	If [port] is not given, it is set to the default port number 
	(see below). *)

  val set_default_port_number : int -> unit
    (** Sets the default port number.
        If not called, the default port number is 51000. *)

  val set_pong_timeout : float -> unit
    (** [set_pong_timeout t] sets the upper time limit [t] for receiving a
	pong message from a worker (since last ping message), before
	we declare the worker unreachable.  If not specified, it
	defaults to 5 seconds. *)

  val set_ping_interval : float -> unit
    (** [set_ping_interval t] sets the interval between consecutive ping
	messages. If not specified, it defaults to 10 seconds. *)

  (** {2 Worker type} *)

  type worker_type = ?port:int -> unit -> unit
    (** The type of forthcoming worker implementations.
	Port number is given by [port]; default value is [51000] and can
	be changed using function [set_default_port_number] above. *)

  type computation_status = Running | Done | Dead

  (** {2 Same binary executed as master and workers}

      A worker is distinguished from the master in two possible ways:
      - either the environment variable WORKER is set and then a worker is 
        immediately started;
      - or function [Worker.compute] below is explicitely called by the
        user program. *)

  module Same : sig

    (** {2 Low level API} *)

    module Computation : sig

      type ('a, 'c) t
	(** The type of distributed computations. *)

      val create : 
	worker:('a -> 'b) -> 
	master:('a * 'c -> 'b -> ('a * 'c) list) ->
	('a, 'c) t
        (** [create worker master] creates a new distributed computation.
	    It has no worker, nor tasks. 
	    Workers (resp.tasks) should be added using function [add_worker]
	    (resp. [add_task]) below. 
	    Note: function [declare_workers] above is only meaningful for 
	    high-level API functions such as [compute], [map], etc. See below.
	*)

      val add_worker : ('a, 'c) t -> worker -> unit
	(** [add_worker c w] adds worker [w] to computation [c]. *)

      val add_task : ('a, 'c) t -> 'a * 'c -> unit
	(** [add_task c t] adds task [t] to computation [c]. *)

      val remove_worker : ('a, 'c) t -> worker -> unit
	(** [remove_worker c w] removes worker [w] from computation [c]. 
	    If [w] was running some task, it will be eventually 
	    rescheduled to another worker. *)

      val one_step : ?timeout:float -> ('a, 'c) t -> unit
	(** [one_step ~timeout c] runs one step of computation [c].
	    That is, it
	    - connects, or reconnects, to workers if necessary;
	    - pings connected workers;
	    - schedule new pending tasks to idle workers if possible;
	    - listens to incoming messages from workers, if any.
	    The optional argument [timeout] indicates how long we should 
	    listen to incoming messages. Its default value is [0] which 
	    means that we handle messages if any, otherwise we immediately 
	    return. A value greater than 0 can be used to avoid busy waiting 
	    in a loop which does nothing but [one_step].
	*)
	
      val status : ('a, 'c) t -> computation_status
	(** [status c] queries the statis of computation [c].
	    It has three possible values:
	    - [Running]: on-going computation.
	    - [Done]: completed computation i.e. there is no pending task and
	      no currently running task. This state could be reached either
	      by completion of all tasks or by function [clear] below.
	    - [Dead]: computation [c] was killed (see function [kill] below).
	*)

      val clear : ('a, 'c) t -> unit
	(** [clear c] clears all tasks from computation [c] i.e. all pending 
	    tasks as well as all currently running tasks. The status of [c]
	    is set to [Done]. One can still add new tasks, which will
	      put [c] back to the [Running] state. *)

      val kill : ('a, 'c) t -> unit
	(** [kill c] kills computation [c]. 
	    It turns the status of [c] to [Dead].
	    [c] cannot be used anymore i.e. any operation applied to [c] will
  	    raise an [Invalid_argument] exception. *)

    end

    (** {2 High level API} *)

    val compute : 
      worker:('a -> 'b) -> 
      master:('a * 'c -> 'b -> ('a * 'c) list) -> ('a * 'c) list -> unit
      
    module Worker : sig
      val compute : worker_type
	(** [compute ()] starts a worker loop, waiting for computations
	    from the master. *)
    end

    (** {2 Derived API} *)
    
    val map : f:('a -> 'b) -> 'a list -> 'b list

    val map_local_fold :
      f:('a -> 'b) -> fold:('c -> 'b -> 'c) -> 'c -> 'a list -> 'c

    val map_remote_fold :
      f:('a -> 'b) -> fold:('c -> 'b -> 'c) -> 'c -> 'a list -> 'c

    val map_fold_ac :
      f:('a -> 'b) -> fold:('b -> 'b -> 'b) -> 'b -> 'a list -> 'b
      
    val map_fold_a :
      f:('a -> 'b) -> fold:('b -> 'b -> 'b) -> 'b -> 'a list -> 'b
      
  end

  (** {2 Polymorphic API (same version of Ocaml)}

      Contrary to module [Same] above, master and workers are no more
      executing the same code. Submodule [Master] (resp. [Worker])
      provides functions to implement the master (resp. the workers).
      Arguments for functions [master], [map], etc. are thus split between
      these two submodules. *)

  module Poly : sig

    module Master : sig

      module Computation : sig
	type ('a, 'c) t
	val create : master:('a * 'c -> 'b -> ('a * 'c) list) -> ('a, 'c) t
	val add_worker : ('a, 'c) t -> worker -> unit
	val remove_worker : ('a, 'c) t -> worker -> unit
	val one_step : ?timeout:float -> ('a, 'c) t -> unit
	val status : ('a, 'c) t -> computation_status
	val kill : ('a, 'c) t -> unit
	val clear : ('a, 'c) t -> unit
	val add_task : ('a, 'c) t -> 'a * 'c -> unit
	val nb_tasks : ('a, 'c) t -> int
      end

      val compute : 
	master:('a * 'c -> 'b -> ('a * 'c) list) -> 
	('a * 'c) list -> unit

      val map : 'a list -> 'b list
      val map_local_fold : fold:('c -> 'b -> 'c) -> 'c -> 'a list -> 'c
      val map_remote_fold : 'c -> 'a list -> 'c
      val map_fold_ac : 'b -> 'a list -> 'b
      val map_fold_a : 'b -> 'a list -> 'b
    end

    module Worker : sig
      val compute : ('a -> 'b) -> worker_type
        
      val map : 
	f:('a -> 'b) -> worker_type
      val map_local_fold : 
	f:('a -> 'b) -> worker_type
      val map_remote_fold :
	f:('a -> 'b) -> fold:('c -> 'b -> 'c) -> worker_type
      val map_fold_ac :
	f:('a -> 'b) -> fold:('b -> 'b -> 'b) -> worker_type
      val map_fold_a :
	f:('a -> 'b) -> fold:('b -> 'b -> 'b) -> worker_type
    end
      
  end

  (** {2 Monomorphic API (possibly different versions of ocaml)} 
      
      When master and workers are not compiled with the same version of Ocaml,
      only strings can be passed, hence the monomorphic API below.
      It is the responsability of user to encode/decode values.

      As of now, there is no derived API in this module. *)

  module Mono : sig

    module Computation : sig
      type 'c t
      val create : 
	master:(string * 'c -> string -> (string * 'c) list) -> 'c t
      val add_worker : 'c t -> worker -> unit
      val remove_worker : 'c t -> worker -> unit
      val one_step : ?timeout:float -> 'c t -> unit
      val status : 'c t -> computation_status
      val kill : 'c t -> unit
      val clear : 'c t -> unit
      val add_task : 'c t -> string * 'c -> unit
    end

    module Master : sig
      val compute : 
	master:(string * 'c -> string -> (string * 'c) list) -> 
	(string * 'c) list -> unit
    end

    module Worker : sig
      val compute : (string -> string) -> worker_type
    end

  end

end

(** Library parameters *)
module Control : sig

  val set_debug : bool -> unit
    (** Sets the debug flag. When set, several messages are displayed
        on error output. (Workers and master display different kinds of
	messages.) *)

end

