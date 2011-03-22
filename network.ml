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

open Format
open Unix
open Control

let () = set_debug false

let default_port_number = ref 51000

let set_default_port_number p = default_port_number := p

let pong_timeout = ref 5.

let set_pong_timeout t = pong_timeout := t

let ping_interval = ref 10.

let set_ping_interval t = ping_interval := t

let is_worker = 
  try ignore (Sys.getenv "WORKER"); true with Not_found -> false 

let encode_string_pair (s1, s2) =
  let buf = Buffer.create 1024 in
  Binary.buf_string buf s1;
  Binary.buf_string buf s2;
  Buffer.contents buf
    
let decode_string_pair s =
  let s1, pos = Binary.get_string s 0 in 
  let s2, _ = Binary.get_string s pos in 
  s1, s2

let print_exception fmt = function
  | Unix_error (e, s1, s2) -> 
    fprintf fmt "Unix_error (%s, %S, %S)" (Unix.error_message e) s1 s2
  | e -> 
    fprintf fmt "%s" (Printexc.to_string e)

(** Worker *****************************************************************)

(*
  Worker implementation
  ---------------------
  the worker runs a server which accept connection on the given port
  (see function "compute" below)

  on each accepted connection, it does a double-fork and the grand-child 
  runs function "server_fun" with the computation function "compute"
  and the socket ("cin", "cout") as arguments; 
  we call this ``the worker'' from now on

  the worker has a main loop waiting for messages from the master 
  (using "select" on the socket); each message is managed using
  "handle_message_from_master". Messages Kill, Ping and Stop are handled 
  immediately. 

  An Assign message makes the worker fork a sub-process, say P, which runs the 
  computation using "compute". The worker and P communicate using a pipe.

  When the computation is completed, its result is written to the pipe and P
  terminates.

  The worker, on the other side, checks whether there is something to read 
  from the pipe (with the same call to "select" it is using to read from the
  master). It so, it reads it and send a Completed message to the master.

  BEWARE: the worker cannot simply wait for P to complete using
  waitpid [WNOHANG] pid and then read from the pipe, since for P to complete 
  it must be necessary to read from the pipe; said otherwise, we could have
  a deadlock with the worker waiting for P to complete before reading from
  the pipe, and P waiting for the worker to read from the pipe in order to
  complete (its writing to the pipe)

  ANOTHER CAVEAT: one could have the worker handle several computations in
  parallel (corresponding to the number of cores to use on that machine).
  But then we could have a deadlock between the master and the worker, with
  the master trying to send a large Assign message, waiting for the worker to
  read it, while the worker tries to send a large Completed message, waiting
  for the master to read it.
  We avoid this caveat by having the worker performing only one computation at
  a time, and having several instances of that worker, one for each ``core''.

*)

module Worker = struct

  let computations : (string, (string -> string)) Hashtbl.t = Hashtbl.create 17

  let register_computation = Hashtbl.add computations
    
  let register_computation2 n f =
    register_computation n
      (fun s -> let x, y = decode_string_pair s in f x y)

  type running_task = {
    id : int;          (* task ID, passed by the master *)
    pid : int;         (* Unix process ID for the computation *)
    file : file_descr; (* the socket to read the result from *)
  }

  open Protocol

  exception ExitOnStop of string

  let server_fun compute cin cout =
    dprintf "new connection@.";
    let old_sigpipe_handler = Sys.signal Sys.sigpipe Sys.Signal_ignore in
    let fdin = descr_of_in_channel cin in
    let fdout = descr_of_out_channel cout in
    let running_task = ref (None : running_task option) in
    let handle_message_from_master () = 
      let m = Master.receive fdin in
      dprintf "received: %a@." Master.print m;
      match m with
	| Master.Assign (id, f, a) ->
	    let fin, fout = pipe () in
	    begin match fork () with
	      | 0 -> 
		  begin try
		    (* FIXME: catch exceptions here *)
		    close fin;
		    (* perform computation *)
		    dprintf "  id %d: computation is running...@." id;
		    let r : string = compute f a in
		    let c = out_channel_of_descr fout in
		    dprintf "  id %d: starting output_value with length %d@." 
		      id (String.length r); 
		    output_value c r;
		    dprintf "  id %d: output_value done@." id;
		    flush c;
		    close_out c;
		    dprintf "  id %d: computation done@." id;
		    exit 0
		  with e ->
		    dprintf "cannot execute job %d (%s)@." 
		      id (Printexc.to_string e);
		    exit 1
		  end
	      | pid -> 
		  assert (!running_task = None);
		  close fout;
		  let t = { id = id; pid = pid; file = fin } in
		  running_task := Some t
	    end
	| Master.Kill id ->
	    begin match !running_task with
	      | None ->
		  ()
	      | Some t ->
		  kill t.pid Sys.sigkill;
  		  running_task := None
	    end
	| Master.Stop r ->
	    raise (ExitOnStop r)
	| Master.Ping ->
	    Worker.send fdout Worker.Pong
    in
    let robust_waitpid pid = try Some (waitpid [WNOHANG] pid) with _ -> None in
    let wait_for_completed_task t = (* only to handle failures *)
      match robust_waitpid t.pid with
	| None 
	| Some (0, _) (* not yet completed *)
	| Some (_, WEXITED 0) (* already completed *) -> 
	    ()
	| Some _ -> (* failure *)
	    running_task := None;
	    Worker.send fdout (Worker.Aborted t.id)
    in
    (* there's something to read from fd *)
    let handle fd =
      if fd == fdin then begin
	handle_message_from_master ()
      end else begin 
	match !running_task with
	  | None -> 
	      assert false
	  | Some t ->
	      let c = in_channel_of_descr t.file in
	      let r : string = input_value c in
	      close_in c;
	      Worker.send fdout (Worker.Completed (t.id, r));
	      running_task := None;
	      ignore (robust_waitpid t.pid)
      end
    in
    (* main loop *)
    try 
      while true do 
	let l = match !running_task with
	  | None -> [fdin]
	  | Some { file = fd } -> [fdin; fd]
	in
	let l,_,_ = select l [] [] 1. in
	List.iter handle l;
	match !running_task with 
	  | None -> () 
	  | Some t -> wait_for_completed_task t
      done;
      assert false
    with 
      | End_of_file -> 
	  dprintf "master disconnected@."; 
  	  begin match !running_task with
	    | None -> ()
	    | Some t -> kill t.pid Sys.sigkill
	  end;
	  exit 0 
      | ExitOnStop r ->
	  ignore (Sys.signal Sys.sigpipe old_sigpipe_handler);
	  r
      | e -> 
	  let s = match e with
	    | Unix_error (e, f, x) -> 
		sprintf "%s (%s, %s)" (error_message e) f x
	    | e -> 
		Printexc.to_string e
	  in
	  eprintf "anomaly: %s@." s; 
	  exit 1

  (* sockets are allocated lazily; this table maps port numbers to sockets *)
  let sockets = Hashtbl.create 17

  let get_socket port =
    try
      Hashtbl.find sockets port
    with Not_found ->
      let sock = socket PF_INET SOCK_STREAM 0 in
      let sockaddr = Unix.ADDR_INET (inet_addr_any, port) in
      setsockopt sock SO_REUSEADDR true;
      setsockopt sock SO_KEEPALIVE true;
      bind sock sockaddr;
      listen sock 3;
      Hashtbl.add sockets port sock;
      sock

  let sockets_fd = Hashtbl.create 17

  let get_socket_fd port =
    let sock = get_socket port in
    try
      Hashtbl.find sockets_fd port
    with Not_found -> 
      let s,_ = Unix.accept sock in
      Hashtbl.add sockets_fd port s;
      s

  let compute compute ?(port = !default_port_number) () = 
    dprintf "port = %d@." port;
    let sock = get_socket port in
    while true do
      let s, _ = Unix.accept sock in 
      match Unix.fork() with
	| 0 -> 
	    if Unix.fork() <> 0 then exit 0; 
            let inchan = Unix.in_channel_of_descr s 
            and outchan = Unix.out_channel_of_descr s in 
	    ignore (server_fun compute inchan outchan);
            begin try close_in inchan; close_out outchan with _ -> () end;
            exit 0
	| id -> 
	    Unix.close s;
	    ignore (Unix.waitpid [] id)
    done;
    assert false

end

(** Master *****************************************************************)

(* 
  Master implementation 

  Each available worker is represented as a record of type "worker" below.
  Each worker is given a distinct ID, stored in field "worker_id".
  The field "sockaddr" stores the connection socket, to be used to (re)connect.
  Fields "fdin" and "fdout" are only meaningful for a connected worker, and
  contain the socket to communicate with the worker.
  The field "job" contains either None, for an idle worker, or Some j for a 
  worker currently perfoming the task with ID j (tasks have IDs, distinct
  from worker IDs).

  New workers are declared with function "declare_workers" and stored
  in the global list "workers".

  The master loop is implemented in function "main_master".
  It maintains a queue of tasks to do (todo) and a set of idle workers 
  (idle_workers). It loops as long as there is some task to do or
  some task currently running, with the following 3 steps.

  The first step is to check and update the status of each worker, 
  which could be:
  - Disconnected: we try to connect and, if so, declare the worker idle
  - Pinged: we look for a possible timeout and, if so, remove from idle workers
    and reschedule its task, if any
  - Ok/Error: we send ping, if appropriate

  The second step is to start new tasks, if possible: if there is some idle
  worker and some task to do, we try to assign the latter to the former 
  (with "create_job"). On failure, we backtrack and put the task back into the
  todo queue.

  The last step consists in listening to workers, using "select".
  For each worker returned by select, we call "listen_for_worker", which
  does the following. First, it set the status to Ok, since we just received
  a message. Then it handles the message, which can be:
  - Pong: if idle, the worker is put back in the idle worker set
          (most likely it was already in that set)
  - Completed: we make the worker idle (with "make_idle") and update the task
               status; in particular we kill all other jobs for that task, if
               any (it could have been rescheduled)
  - Aborted: we make the worker idle and put the task back into the todo queue

*)

type worker_state =
  | Disconnected
  | Ok     of float (* time since last pong (or initial connection time) *)
  | Pinged of float (* last time we pinged *)
  | Error  of float (* last time we pinged *)

type worker = { 
  worker_id : int;
  sockaddr : sockaddr;
  mutable state : worker_state;
  mutable fdin : file_descr;
  mutable fdout : file_descr;
  mutable job : int option; (* None means idle / Some j means running job j *)
}

type 'a task = {
  task : 'a;
  mutable task_done : bool;
  mutable task_workers : (int * worker) list; (* job id / worker *)
}

let print_task fmt t=
  fprintf fmt "@[done=%b, workers={" t.task_done;
  List.iter (fun (jid, w) -> fprintf fmt "%d (on worker %d), " jid w.worker_id)
    t.task_workers;
  fprintf fmt "}@]"

let print_sockaddr fmt = function
  | ADDR_UNIX s -> fprintf fmt "%s" s
  | ADDR_INET (ia, port) -> fprintf fmt "%s:%d" (string_of_inet_addr ia) port

let print_worker fmt w = match w.job with
  | None -> 
      fprintf fmt "@[%d @[(%a,@ idle)@]@]" 
      w.worker_id print_sockaddr w.sockaddr 
  | Some j -> 
      fprintf fmt "@[%d @[(%a,@ running task %d)@]@]" 
      w.worker_id print_sockaddr w.sockaddr j

module WorkerSet : sig
  type t
  val create : unit -> t
  val clear : t -> unit
  val add : t -> worker -> unit
  val mem : t -> worker -> bool
  val remove : t -> worker -> unit
  val is_empty : t -> bool
  val choose : t -> worker (* does not remove it *)
  val cardinal : t -> int
  val iter : (worker -> unit) -> t -> unit
end = struct
  module S = 
    Set.Make(struct 
	       type t = worker 
	       let compare w1 w2 = Pervasives.compare w1.worker_id w2.worker_id
	     end)
  type t = S.t ref
  let create () = ref S.empty
  let clear h = h := S.empty
  let add h w = h := S.add w !h
  let mem h w = S.mem w !h
  let remove h w = h := S.remove w !h
  let is_empty h = S.is_empty !h
  let choose h = assert (not (S.is_empty !h)); S.choose !h
  let cardinal h = S.cardinal !h
  let iter f h = S.iter f !h
end

let workers = ref []

let () =
  at_exit 
    (fun () ->
       if not is_worker then
	 let shutdown_worker w = Unix.shutdown w.fdin Unix.SHUTDOWN_SEND in
	 List.iter 
	   (fun w -> if w.state <> Disconnected then shutdown_worker w)
	   !workers)

let create_sock_addr name port =
  let addr = 
    try  
      inet_addr_of_string name
    with Failure "inet_addr_of_string" -> 
      try 
	(gethostbyname name).h_addr_list.(0) 
      with Not_found ->
	invalid_arg (sprintf "%s : Unknown server@." name)
  in
  ADDR_INET (addr, port) 

let create_worker =
  let r = ref 0 in
  fun ?(port = !default_port_number) s ->
    incr r;
    let a = create_sock_addr s port in
    { 
      worker_id = !r;
      sockaddr = a; 
      state = Disconnected;
      fdin = stdin; 
      fdout = stdout;
      job = None; (* idle *)
    } 

let declare_workers ?(port = !default_port_number) ?(n=1) s =
  if n <= 0 then invalid_arg "declare_workers";
  for i = 1 to n do
    workers := create_worker ~port s :: !workers
  done

let worker_fd = Hashtbl.create 17

let connect_worker w =
  if w.state = Disconnected then begin
    let ic,oc = open_connection w.sockaddr in
    let fdin = descr_of_in_channel ic in
    let fdout = descr_of_out_channel oc in
    w.state <- Ok (Unix.time ());
    Hashtbl.remove worker_fd w.fdin;
    w.fdin <- fdin;
    Hashtbl.add worker_fd w.fdin w;
    w.fdout <- fdout;
  end

let create_task t =
  { task = t;
    task_done = false;
    task_workers = []; }

let job_id = let r = ref 0 in fun () -> incr r; !r

type computation_status = Running | Done | Dead

type ('a, 'c) computation_ = {
  mutable status : computation_status;
  assign_job : 'a -> string * string;
  master : 'a * 'c -> string -> ('a * 'c) list;
  old_sigpipe_handler : Sys.signal_behavior;
  todo : ('a * 'c) task Queue.t;
  workers : WorkerSet.t;
  idle_workers : WorkerSet.t; (* subset of workers *)
  running_tasks : (int, ('a * 'c) task) Hashtbl.t;
  mutable last_printed_state : int * int * int;
}

let add_task c t = 
  if c.status = Dead then invalid_arg "add_task: dead computation";
  Queue.add (create_task t) c.todo;
  c.status <- Running

let add_worker c w =
  WorkerSet.add c.workers w;
  match w.state with
  | Ok _ | Pinged _ -> 
      if w.job = None then WorkerSet.add c.idle_workers w
  | Disconnected | Error _ -> 
      ()

let create_computation_
    ~(assign_job : 'a -> string * string)
    ~(master : 'a * 'c -> string -> ('a * 'c) list) 
    =
  let c = { 
    status = Done;
    old_sigpipe_handler = Sys.signal Sys.sigpipe Sys.Signal_ignore;
    assign_job = assign_job;
    master = master;
    todo = Queue.create ();
    workers = WorkerSet.create ();
    idle_workers = WorkerSet.create ();
    running_tasks = Hashtbl.create 17;
    last_printed_state = 0,0,0;
  }
  in
  c

let print_computation c = match c.status with
  | Dead ->
      dprintf "dead computation"
  | Done ->
      dprintf "computation done"
  | Running ->
      let n1 = Queue.length c.todo in
      let n2 = WorkerSet.cardinal c.idle_workers in
      let n3 = Hashtbl.length c.running_tasks in
      let st = (n1, n2, n3) in 
      if st <> c.last_printed_state then begin
	c.last_printed_state <- st;
	dprintf "***@.";
	dprintf "  %d tasks todo@." n1;
	dprintf "  %d idle workers@." n2;
	dprintf "  %d running tasks (" n3;
	Hashtbl.iter (fun jid _ -> dprintf "%d, " jid) c.running_tasks;
	dprintf ")@.";
	dprintf "***@.";
      end

let send w m =
  try 
    Protocol.Master.send w.fdout m
  with e -> 
    dprintf "@[<hov 2>could not send message %a@ to worker %a@ (%a)@]@." 
      Protocol.Master.print m print_worker w print_exception e;
    raise e

let make_idle c w =
  w.job <- None;
  WorkerSet.add c.idle_workers w 

(* kill job jid on worker w *)
let kill_job c w jid =
  dprintf "kill job id %d on worker %d@." jid w.worker_id;
  Hashtbl.remove c.running_tasks jid;
  if w.state <> Disconnected then begin
    send w (Protocol.Master.Kill jid);
    make_idle c w
  end

let reschedule_task c ~remove w = match w.job with
  | None ->
      () (* may be idle *)
  | Some jid ->
      assert (Hashtbl.mem c.running_tasks jid);
      let t = Hashtbl.find c.running_tasks jid in
      if remove then begin
	Hashtbl.remove c.running_tasks jid;
	t.task_workers <- List.filter (fun (_,w') -> w' != w) t.task_workers
      end;
      Queue.add t c.todo

let manage_disconnection c w =
  begin match w.job with
    | Some jid when w.state <> Disconnected -> send w (Protocol.Master.Kill jid)
    | _ -> ()
  end;
  w.state <- Disconnected;
  WorkerSet.remove c.idle_workers w;
  reschedule_task c ~remove:true w

let remove_worker c w =
  manage_disconnection c w;
  WorkerSet.remove c.workers w

let do_one_step ?(timeout=0.) c =
  let send_ping w = send w Protocol.Master.Ping in
  let create_job w t =
    assert (not t.task_done);
    dprintf "@[<hov 2>create_job: worker=%a,@ task=%a@]@." 
      print_worker w print_task t;
    connect_worker w;
    let id = job_id () in
    let f, a = c.assign_job (fst t.task) in
    send w (Protocol.Master.Assign (id, f, a));
    send_ping w;
    w.state <- Pinged (Unix.time ());
    t.task_workers <- (id, w)  :: t.task_workers;
    Hashtbl.replace c.running_tasks id t;
    w.job <- Some id;
    WorkerSet.remove c.idle_workers w
  in
  (* kill jobs different from jid *)
  let kill_jobs jid l =
    List.iter (fun (jid', w) -> if jid' <> jid then kill_job c w jid') l
  in
  let listen_for_worker w =
    let m = Protocol.Worker.receive w.fdin in
    dprintf "received from %a: %a@." print_worker w Protocol.Worker.print m;
    w.state <- Ok (Unix.time ());
    match m with
      | Protocol.Worker.Pong ->
	  if w.job = None then WorkerSet.add c.idle_workers w
      | Protocol.Worker.Completed (id, r) ->
	  make_idle c w;
	  let t = Hashtbl.find c.running_tasks id in
	  dprintf "completed task: job id=%d, %a@." id print_task t;
	  Hashtbl.remove c.running_tasks id;
	  if not t.task_done then begin
	    t.task_done <- true;
	    kill_jobs id t.task_workers;
	    List.iter (add_task c) (c.master t.task r)
	  end 
      | Protocol.Worker.Aborted id ->
	  make_idle c w;
	  let t = Hashtbl.find c.running_tasks id in
	  Hashtbl.remove c.running_tasks id;
	  add_task c t.task
  in
  print_computation c;
  (* 1. try to connect if not already connected *)
  let current = Unix.time () in
  WorkerSet.iter 
    (fun w -> match w.state with
       | Disconnected ->
	   begin try 
	     connect_worker w;
	     dprintf "new connection to %a@." print_worker w;
	     make_idle c w
	   with e ->
	     ()
	   end
       | Pinged t ->
	   if current > t +. !pong_timeout then begin
	     dprintf "worker %a timed out@." print_worker w;
	     w.state <- Error current;
	     WorkerSet.remove c.idle_workers w;
	     reschedule_task c ~remove:false w
	   end
       | Ok t when current > t +. !ping_interval ->
	   send_ping w;
	   w.state <- Pinged current
       | Error t when current > t +. !ping_interval ->
	   send_ping w;
	   w.state <- Error current
       | Ok _ | Error _ ->
	   ())
    c.workers;
  print_computation c;
  (* 2. if possible, start new jobs *)
  while not (WorkerSet.is_empty c.idle_workers) && not (Queue.is_empty c.todo) 
  do
    let t = Queue.pop c.todo in
    if not t.task_done then begin
      let w = WorkerSet.choose c.idle_workers in
      try 
	create_job w t 
      with e -> 
	dprintf "@[<hov 2>create_job for worker %a failed:@ %a@]"
	  print_worker w print_exception e;
	Queue.push t c.todo;
	manage_disconnection c w
    end
  done;
  print_computation c;
  (* 3. if not, listen for workers *)
  let fds = 
    let wl = ref [] in
    WorkerSet.iter 
      (fun w -> if w.state <> Disconnected then wl := w :: !wl) 
      c.workers;
    List.map (fun w -> w.fdin) !wl
  in
  let l,_,_ = select fds [] [] timeout in
  List.iter 
    (fun fd -> 
       let w = Hashtbl.find worker_fd fd in
       try  
	 listen_for_worker w
       with e -> 
	 dprintf "@[<hov 2>worker %a failure:@ %s@]@." 
	   print_worker w (Printexc.to_string e);
	 manage_disconnection c w)
    l
  
let one_step ?timeout c = match c.status with
  | Dead ->
      invalid_arg "one_step: dead computation"
  | Done ->
      ()
  | Running when Queue.is_empty c.todo && Hashtbl.length c.running_tasks = 0 ->
      (* we are done *)
      c.status <- Done;
      ignore (Sys.signal Sys.sigpipe c.old_sigpipe_handler)
  | Running ->
      do_one_step ?timeout c

let status c = c.status

let nb_tasks c = Queue.length c.todo

let clear c =
  let killed = Hashtbl.create 17 in
  let kill (jid, w) = 
    if not (Hashtbl.mem killed jid) then begin 
      kill_job c w jid; 
      Hashtbl.add killed jid ()
    end
  in
  Hashtbl.iter (fun _ t -> List.iter kill t.task_workers) c.running_tasks;
  Queue.clear c.todo;
  Hashtbl.clear c.running_tasks;
  c.status <- Done

let kill c =
  clear c;
  let stop_msg = Protocol.Master.Stop "kill computation" in
  WorkerSet.iter (fun w -> send w stop_msg) c.idle_workers;
  WorkerSet.clear c.idle_workers;
  c.status <- Dead

let main_master 
    ~(assign_job : 'a -> string * string)
    ~(master : 'a * 'c -> string -> ('a * 'c) list) 
    (tasks : ('a * 'c) list)
    =
  let c = create_computation_ ~assign_job ~master in
  List.iter (add_worker c) !workers;
  List.iter (add_task c) tasks;
  while c.status = Running do
    one_step ~timeout:0.1 c
  done;
  kill c

(*****
let main_master 
    ~(assign_job : 'a -> string * string)
    ~(master : 'a * 'c -> string -> ('a * 'c) list) 
    (tasks : ('a * 'c) list)
    =
  let old_sigpipe_handler = Sys.signal Sys.sigpipe Sys.Signal_ignore in
  (* the tasks still to be done *)
  let todo = Queue.create () in
  let push_new_task t = Queue.add (create_task t) todo in
  List.iter push_new_task tasks;
  (* idle workers *)
  let idle_workers = WorkerSet.create () in
  List.iter 
    (fun w -> match w.state with
       | Ok _ | Pinged _ -> 
	   if w.job = None then WorkerSet.add idle_workers w
       | Disconnected | Error _ -> 
	   ())
    !workers;
  (* running tasks (job id -> task) *)
  let running_tasks = Hashtbl.create 17 in
  let send w m =
    try 
      Protocol.Master.send w.fdout m
    with e -> 
      dprintf "@[<hov 2>could not send message %a@ to worker %a@ (%a)@]@." 
	Protocol.Master.print m print_worker w print_exception e;
      raise e
  in
  let send_ping w = send w Protocol.Master.Ping in
  let create_job w t =
    assert (not t.task_done);
    dprintf "@[<hov 2>create_job: worker=%a,@ task=%a@]@." 
      print_worker w print_task t;
    connect_worker w;
    let id = job_id () in
    let f, a = assign_job (fst t.task) in
    send w (Protocol.Master.Assign (id, f, a));
    send_ping w;
    w.state <- Pinged (Unix.time ());
    t.task_workers <- (id, w)  :: t.task_workers;
    Hashtbl.replace running_tasks id t;
    w.job <- Some id;
    WorkerSet.remove idle_workers w
  in
  let reschedule_task ~remove w = match w.job with
    | None ->
        () (* may be idle *)
    | Some jid ->
        assert (Hashtbl.mem running_tasks jid);
        let t = Hashtbl.find running_tasks jid in
	if remove then begin
	  Hashtbl.remove running_tasks jid;
	  t.task_workers <- List.filter (fun (_,w') -> w' != w) t.task_workers
	end;
	Queue.add t todo
  in
  let manage_disconnection w =
    w.state <- Disconnected;
    WorkerSet.remove idle_workers w;
    reschedule_task ~remove:true w
  in
  let make_idle w =
    w.job <- None;
    WorkerSet.add idle_workers w 
  in
  (* kill job jid on worker w *)
  let kill w jid =
    dprintf "kill job id %d on worker %d@." jid w.worker_id;
    Hashtbl.remove running_tasks jid;
    if w.state <> Disconnected then begin
      send w (Protocol.Master.Kill jid);
      make_idle w
    end
  in
  (* kill jobs different from jid *)
  let kill_jobs jid l =
    List.iter (fun (jid', w) -> if jid' <> jid then kill w jid') l
  in
  let listen_for_worker w =
    let m = Protocol.Worker.receive w.fdin in
    dprintf "received from %a: %a@." print_worker w Protocol.Worker.print m;
    w.state <- Ok (Unix.time ());
    match m with
      | Protocol.Worker.Pong ->
	  if w.job = None then WorkerSet.add idle_workers w
      | Protocol.Worker.Completed (id, r) ->
	  make_idle w;
	  let t = Hashtbl.find running_tasks id in
	  dprintf "completed task: job id=%d, %a@." id print_task t;
	  Hashtbl.remove running_tasks id;
	  if not t.task_done then begin
	    t.task_done <- true;
	    kill_jobs id t.task_workers;
	    List.iter push_new_task (master t.task r)
	  end 
      | Protocol.Worker.Aborted id ->
	  make_idle w;
	  let t = Hashtbl.find running_tasks id in
	  Hashtbl.remove running_tasks id;
	  push_new_task t.task
  in
  let last_printed_state = ref (0,0,0) in
  let print_state () =
    let n1 = Queue.length todo in
    let n2 = WorkerSet.cardinal idle_workers in
    let n3 = Hashtbl.length running_tasks in
    let st = (n1, n2, n3) in 
    if st <> !last_printed_state then begin
      last_printed_state := st;
      dprintf "***@.";
      dprintf "  %d tasks todo@." n1;
      dprintf "  %d idle workers@." n2;
      dprintf "  %d running tasks (" n3;
      Hashtbl.iter (fun jid _ -> dprintf "%d, " jid) running_tasks;
      dprintf ")@.";
      dprintf "***@.";
    end
  in
  (* main loop *)
  while not (Queue.is_empty todo) || (Hashtbl.length running_tasks > 0) do

    print_state ();

    (* 1. try to connect if not already connected *)
    let current = Unix.time () in
    List.iter 
      (fun w -> match w.state with
	 | Disconnected ->
	     begin try 
	       connect_worker w;
	       dprintf "new connection to %a@." print_worker w;
	       make_idle w
	     with e ->
	       ()
	     end
	 | Pinged t ->
	     if current > t +. !pong_timeout then begin
	       dprintf "worker %a timed out@." print_worker w;
	       w.state <- Error current;
	       WorkerSet.remove idle_workers w;
	       reschedule_task ~remove:false w
	     end
	 | Ok t when current > t +. !ping_interval ->
	     send_ping w;
	     w.state <- Pinged current
	 | Error t when current > t +. !ping_interval ->
	     send_ping w;
	     w.state <- Error current
	 | Ok _ | Error _ ->
	     ())
      !workers;

    print_state ();

    (* 2. if possible, start new jobs *)
    while not (WorkerSet.is_empty idle_workers) && not (Queue.is_empty todo) do
      let t = Queue.pop todo in
      if not t.task_done then begin
	let w = WorkerSet.choose idle_workers in
	try 
	  create_job w t 
	with e -> 
	  dprintf "@[<hov 2>create_job for worker %a failed:@ %a@]"
	    print_worker w print_exception e;
	  Queue.push t todo;
	  manage_disconnection w
      end
    done;

    print_state ();

    (* 3. if not, listen for workers *)
    let fds = 
      let wl = List.filter (fun w -> w.state <> Disconnected) !workers in
      List.map (fun w -> w.fdin) wl
    in
    let l,_,_ = select fds [] [] 0.1 in
    List.iter 
      (fun fd -> 
	 let w = Hashtbl.find worker_fd fd in
	 try  
	   listen_for_worker w
	 with e -> 
	   dprintf "@[<hov 2>worker %a failure:@ %s@]@." 
	     print_worker w (Printexc.to_string e);
	   manage_disconnection w)
      l;

  done;
  assert (Queue.is_empty todo && Hashtbl.length running_tasks = 0);
  ignore (Sys.signal Sys.sigpipe old_sigpipe_handler);
  ()
****)

(** Three implementations ***************************************************)

type worker_type = ?port:int -> unit -> unit

module Mono = struct

  module Computation = struct

    type 'c t = (string, 'c) computation_

    let create ~master =
      let assign_job x = "f", x in
      create_computation_ ~assign_job ~master 

    let add_worker = add_worker
    let remove_worker = remove_worker
    let status = status
    let one_step = one_step
    let add_task = add_task
    let kill = kill
    let clear = clear

  end

  module Master = struct

    let compute ~master tl =
      main_master ~assign_job:(fun x -> "f", x) ~master tl
	
  end

  module Worker = struct
    let compute f ?port () = 
      ignore (Worker.compute (fun _ x -> f x) ?port ())
  end

end

module Poly = struct

  module Master = struct

    module Computation = struct
      type ('a, 'c) t = ('a, 'c) computation_

      let create ~(master : 'a * 'c -> 'b -> ('a * 'c) list) = 
	let assign_job x = "f", Marshal.to_string x [] in
	let master t s = let r = Marshal.from_string s 0 in master t r in
	create_computation_ ~assign_job ~master

      let add_worker = add_worker
      let remove_worker = remove_worker
      let status = status
      let one_step = one_step
      let add_task = add_task
      let kill = kill
      let clear = clear
      let nb_tasks = nb_tasks

    end

    let compute ~master tl =
      main_master 
	~assign_job:(fun x -> "f", Marshal.to_string x []) 
	~master:(fun t s -> let r = Marshal.from_string s 0 in master t r)
	tl
	
    include Map_fold.Make
	(struct
	   let compute ~worker = compute
	 end)
    let map l = 
      map ~f:(fun _ -> assert false) l
    let map_local_fold ~fold acc l = 
      map_local_fold
	~f:(fun _ -> assert false) ~fold acc l
    let map_remote_fold acc l = 
      map_remote_fold
	~f:(fun _ -> assert false) ~fold:(fun _ _ -> assert false) acc l
    let map_fold_ac acc l = 
      map_fold_ac
	~f:(fun _ -> assert false) ~fold:(fun _ _ -> assert false) acc l
    let map_fold_a acc l = 
      map_fold_a
	~f:(fun _ -> assert false) ~fold:(fun _ _ -> assert false) acc l

  end

  module Worker = struct
    let unpoly f s = 
      let x = Marshal.from_string s 0 in
      let r = f x in
      Marshal.to_string r []

    let compute f ?port () = 
      ignore (Worker.compute (fun _ -> unpoly f) ?port ())

    let map ~f = 
      compute f
    let map_local_fold ~f = 
      compute f
    let map_remote_fold ~f ~fold = 
      compute (Map_fold.map_fold_wrapper f fold)
    let map_fold_ac ~f ~fold = 
      compute (Map_fold.map_fold_wrapper2 f fold)
    let map_fold_a ~f ~fold = 
      compute (Map_fold.map_fold_wrapper2 f fold)
  end

end

module Same = struct

  module Worker = struct 

    let compute ?port () =
      let compute f x = 
	let f = (Marshal.from_string f 0 : 'a -> 'b) in
	let x = (Marshal.from_string x 0 : 'a) in
	Marshal.to_string (f x) []
      in
      ignore (Worker.compute compute ?port ())

  end

  module Computation = struct

    type ('a, 'c) t = ('a, 'c) computation_

    let create 
	~(worker : 'a -> 'b) 
	~(master : 'a * 'c -> 'b -> ('a * 'c) list) 
    = 
      let worker_closure = Marshal.to_string worker [Marshal.Closures] in
      let assign_job x = worker_closure, Marshal.to_string x [] in
      let master ac r = master ac (Marshal.from_string r 0) in
      create_computation_ ~assign_job ~master

    let add_worker = add_worker
    let remove_worker = remove_worker
    let status = status
    let one_step = one_step
    let add_task = add_task
    let kill = kill
    let clear = clear

  end

  let () = 
    if is_worker then begin
      dprintf "starting worker loop...@.";
      Worker.compute () (* never returns *)
    end
      
  let compute
      ~(worker : 'a -> 'b) 
      ~(master : 'a * 'c -> 'b -> ('a * 'c) list) 
      tasks
  =
    let worker_closure = Marshal.to_string worker [Marshal.Closures] in
    let assign_job x = worker_closure, Marshal.to_string x [] in
    let master ac r = master ac (Marshal.from_string r 0) in
    main_master ~assign_job ~master tasks
      
  include Map_fold.Make(struct let compute = compute end)

 end

