(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) Jean-Christophe Filliatre and Kalyan Krishnamani        *)
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

let encode_string_pair (s1, s2) =
  let buf = Buffer.create 1024 in
  Binary.buf_string buf s1;
  Binary.buf_string buf s2;
  Buffer.contents buf
    
let decode_string_pair s =
  let s1, pos = Binary.get_string s 0 in 
  let s2, _ = Binary.get_string s pos in 
  s1, s2

module Worker = struct

  let computations : (string, (string -> string)) Hashtbl.t = Hashtbl.create 17

  let register_computation = Hashtbl.add computations
    
  let register_computation2 n f =
    register_computation n
      (fun s -> let x, y = decode_string_pair s in f x y)

  type running_task = {
    pid : int;
    file : file_descr;
  }

  open Protocol

  exception ExitOnStop of string

(*     if Hashtbl.mem computations f then begin *)
(* 	      let f = Hashtbl.find computations f in *)

  let server_fun compute cin cout =
    dprintf "new connection@.";
    let fdin = descr_of_in_channel cin in
    let fdout = descr_of_out_channel cout in
    let pids = Hashtbl.create 17 in (* ID -> running_task *)
    let handle_message_from_master _ = 
      let m = Master.receive fdin in
      dprintf "received: %a@." Master.print m;
      match m with
	| Master.Assign (id, f, a) ->
	    begin try
	      Worker.send fdout (Worker.Started id);
	      let fin, fout = pipe () in
	      begin match fork () with
		| 0 -> 
		    close fin;
		    (* perform computation *)
		    dprintf "  id %d: computation is running...@." id;
		    let r : string = compute f a in
		    let c = out_channel_of_descr fout in
		    output_value c r;
		    dprintf "  id %d: computation done@." id;
		    exit 0
		| pid -> 
		    close fout;
		    let t = { pid = pid; file = fin } in
		    Hashtbl.add pids id t
	      end
	    with e ->
	      dprintf "cannot execute job %d (%s)@." id (Printexc.to_string e);
	      Worker.send fdout (Worker.Aborted id)
	    end
	| Master.Kill id ->
	    begin 
	      try
		let t = Hashtbl.find pids id in
		kill t.pid Sys.sigkill;
		Hashtbl.remove pids id
	      with Not_found ->
		() (* ignored Kill *)
	    end
	| Master.Stop r ->
	    raise (ExitOnStop r)
    in
    let wait_for_completed_task id t =
      match waitpid [WNOHANG] t.pid with
	| 0, _ -> (* not yet completed *)
	    ()
	| _, WEXITED _ -> (* success FIXME: check return code *)
	    Hashtbl.remove pids id;
	    let c = in_channel_of_descr t.file in
	    let r : string = input_value c in
	    close_in c;
	    Worker.send fdout (Worker.Completed (id, r))
	| _, (WSIGNALED _ | WSTOPPED _) -> (* failure *)
	    Hashtbl.remove pids id;
	    Worker.send fdout (Worker.Aborted id)
    in
    try 
      while true do    
	let l,_,_ = select [fdin] [] [] 1. in
	List.iter handle_message_from_master l;
	Hashtbl.iter wait_for_completed_task pids
      done;
      assert false
    with 
      | End_of_file -> 
	  dprintf "master disconnected@."; 
	  Hashtbl.iter (fun _ t -> kill t.pid Sys.sigkill) pids;
	  exit 0 
      | ExitOnStop r ->
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

(*   let () =  *)
(*     at_exit  *)
(*       (fun () -> *)
(* 	 Hashtbl.iter *)
(* 	   (fun _ s ->  *)
(* 	      begin try  *)
(* 		shutdown s SHUTDOWN_ALL *)
(* 	      with e ->  *)
(* 		eprintf "cannot shutdown socket: %s@." (Printexc.to_string e)  *)
(* 	      end) *)
(* 	   sockets) *)
      
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

  let compute compute ?(stop=false) ?(port=51000) () = 
    if stop then begin
      let s = get_socket_fd port in
      let inchan = Unix.in_channel_of_descr s 
      and outchan = Unix.out_channel_of_descr s in 
      server_fun compute inchan outchan 
    end else begin
      let sock = get_socket port in
      while true do
	let s, _ = Unix.accept sock in 
	match Unix.fork() with
	  | 0 -> 
	      if Unix.fork() <> 0 then exit 0; 
              let inchan = Unix.in_channel_of_descr s 
              and outchan = Unix.out_channel_of_descr s in 
	      ignore (server_fun compute inchan outchan);
              close_in inchan;
              close_out outchan;
              exit 0
	  | id -> 
	      Unix.close s;
	      ignore (Unix.waitpid [] id)
      done;
      assert false
    end

end

(** Master *)

module IntSet = 
  Set.Make(struct type t = int let compare = Pervasives.compare end)

type worker = { 
  worker_id : int;
  sockaddr : sockaddr;
  mutable connected : bool;
  mutable error : bool;
  mutable fdin : file_descr;
  mutable fdout : file_descr;
  ncores : int;
  mutable idle_cores : int;
  mutable jobs : IntSet.t;
}

type 'a task = {
  task : 'a;
  task_id : int;
  mutable task_workers : (int * worker) list; (* job id / worker *)
}

let print_sockaddr fmt = function
  | ADDR_UNIX s -> fprintf fmt "%s" s
  | ADDR_INET (ia, port) -> fprintf fmt "%s:%d" (string_of_inet_addr ia) port

let print_worker fmt w = 
  fprintf fmt "@[%a (%d cores, %d idle cores, %d jobs)@]" 
    print_sockaddr w.sockaddr 
    w.ncores w.idle_cores (IntSet.cardinal w.jobs)

module WorkerSet : sig
  type t
  val create : unit -> t
  val add : t -> worker -> unit
  val mem : t -> worker -> bool
  val remove : t -> worker -> unit
  val is_empty : t -> bool
  val choose : t -> worker (* does not remove it *)
end = struct
  module S = 
    Set.Make(struct 
	       type t = worker 
	       let compare w1 w2 = Pervasives.compare w1.worker_id w2.worker_id
	     end)
  type t = S.t ref
  let create () = ref S.empty
  let add h w = h := S.add w !h
  let mem h w = S.mem w !h
  let remove h w = h := S.remove w !h
  let is_empty h = S.is_empty !h
  let choose h = assert (not (S.is_empty !h)); S.choose !h
end

let workers = ref []

let create_sock_addr name port =
  let addr = 
    try  
      inet_addr_of_string name
    with Failure "inet_addr_of_string" -> 
      try 
	(gethostbyname name).h_addr_list.(0) 
      with Not_found ->
	eprintf "%s : Unknown server@." name ;
	exit 1
  in
  ADDR_INET (addr, port) 

let declare_workers =
  let r = ref 0 in
  fun ?(port=51000) ?(n=1) s ->
    let a = create_sock_addr s port in
    let w = { 
      worker_id = !r;
      sockaddr = a; 
      connected = false; 
      error = false;
      fdin = stdin; 
      fdout = stdout;
      ncores = n;
      idle_cores = n;
      jobs = IntSet.empty;
    } 
    in
    workers := w :: !workers

let connect_worker w =
  if not w.connected then begin
    let ic,oc = open_connection w.sockaddr in
    let fdin = descr_of_in_channel ic in
    let fdout = descr_of_out_channel oc in
    w.connected <- true;
    w.fdin <- fdin;
    w.fdout <- fdout
  end

module TaskSet : sig
  type 'a t
  val create : unit -> 'a t
  val add : 'a t -> 'a task -> unit
  val mem : 'a t -> 'a task -> bool
  val remove : 'a t -> 'a task -> unit
  val is_empty : 'a t -> bool
  val cardinal : 'a t -> int
end = struct
  type 'a t = (int, unit) Hashtbl.t
  let create () = Hashtbl.create 17
  let add h t = Hashtbl.add h t.task_id ()
  let mem h t = Hashtbl.mem h t.task_id
  let remove h t = Hashtbl.remove h t.task_id
  let is_empty h = 
    try Hashtbl.iter (fun _ _ -> raise Exit) h; true
    with Exit -> false
  let cardinal h = Hashtbl.length h
end

let task_counter = ref 0
let create_task t =
  incr task_counter; 
  { task = t;
    task_id = !task_counter;
    task_workers = []; }

let job_id = let r = ref 0 in fun () -> incr r; !r

let main_master 
  ~(assign_job : 'a -> string * string)
  ~(handle : 'a * 'c -> string -> ('a * 'c) list) 
  (tasks : ('a * 'c) list)
=
  (* the tasks still to be done *)
  let todo = Stack.create () in
  List.iter (fun t -> Stack.push (create_task t) todo) tasks;
  (* idle workers *)
  let idle_workers = WorkerSet.create () in
  List.iter 
    (fun w -> 
       if w.connected && w.idle_cores > 0 then WorkerSet.add idle_workers w)
    !workers;
  (* running tasks (job id -> task) *)
  let running_tasks = Hashtbl.create 17 in
  let create_job w t =
    connect_worker w;
    let id = job_id () in
    let f, a = assign_job (fst t.task) in
    Protocol.Master.send w.fdout (Protocol.Master.Assign (id, f, a));
    t.task_workers <- (id, w)  :: t.task_workers;
    Hashtbl.replace running_tasks id t;
    w.jobs <- IntSet.add id w.jobs;
    assert (w.idle_cores > 0);
    w.idle_cores <- w.idle_cores - 1;
    if w.idle_cores = 0 then WorkerSet.remove idle_workers w
  in
  let manage_disconnection w =
    w.connected <- false;
    WorkerSet.remove idle_workers w;
    IntSet.iter 
      (fun jid -> 
	 assert (Hashtbl.mem running_tasks jid);
	 let t = Hashtbl.find running_tasks jid in
	 Hashtbl.remove running_tasks jid;
	 t.task_workers <- List.filter (fun (_,w') -> w' != w) t.task_workers;
	 Stack.push t todo)
      w.jobs;
  in
  let increase_idle_cores w =
    w.idle_cores <- w.idle_cores + 1;
    if w.idle_cores = 1 then WorkerSet.add idle_workers w
  in
  (* kill jobs not assigned to w in list l *)
  let kill w jid =
    Protocol.Master.send w.fdout (Protocol.Master.Kill jid);
    w.jobs <- IntSet.remove jid w.jobs;
    increase_idle_cores w
  in
  let kill_jobs w l =
    List.iter 
      (fun (jid', w') -> if w'.worker_id <> w.worker_id then kill w' jid') l
  in
  let wait () =
    let listen_for_worker w =
      let l,_,_ = select [w.fdin] [] [] 0.1 in
      if l = [] then raise Exit;
      dprintf "ready to receive@.";
      let m = Protocol.Worker.receive w.fdin in
      dprintf "received from %a: %a@." print_worker w Protocol.Worker.print m;
      match m with
	| Protocol.Worker.Started _ ->
	    raise Exit
	| Protocol.Worker.Completed (id, r) ->
	    let t = Hashtbl.find running_tasks id in
	    Hashtbl.remove running_tasks id;
	    kill_jobs w t.task_workers;
	    w.jobs <- IntSet.remove id w.jobs;
	    increase_idle_cores w;
	    w, handle t.task r
	| Protocol.Worker.Aborted id ->
	    let t = Hashtbl.find running_tasks id in
	    Hashtbl.remove running_tasks id;
	    w.jobs <- IntSet.remove id w.jobs;
	    increase_idle_cores w;
	    w, [t.task]
    in
    (* loop over workers, until one has a message for us *)
    let rec loop = function
      | [] -> 
	  raise Exit
      | w :: wl when not w.connected ->
	  loop wl
      | w :: wl -> 
	  try 
	    listen_for_worker w
	  with 
	    | Exit -> loop wl
	    | End_of_file -> manage_disconnection w; raise Exit
    in
    loop !workers
  in
  (* main loop *)
  while not (Stack.is_empty todo) || (Hashtbl.length running_tasks > 0) do

    (* try to connect if not already connected *)
    List.iter 
      (fun w -> if not w.connected then begin
	 try 
	   connect_worker w;
	   WorkerSet.add idle_workers w;
	   w.idle_cores <- w.ncores; (* we assume all cores are back *)
	   w.jobs <- IntSet.empty;
	 with e ->
	   ()
       end) 
      !workers;

    (* if possible, start a new job *)
    while not (WorkerSet.is_empty idle_workers) && not (Stack.is_empty todo) do
      let t = Stack.pop todo in
      assert (t.task_workers = []); (* TO BE REMOVED EVENTUALLY *)
      let w = WorkerSet.choose idle_workers in
      create_job w t
    done;

    (***
    printf "----@.";
    printf "%d running tasks, %d tasks to do@." 
      (Hashtbl.length running_tasks) (Stack.length todo);
    ***)	

    (* if not, wait for any message from the workers *)
    if Hashtbl.length running_tasks > 0 then begin
      try
	let w, tl = wait () in
	List.iter (fun t -> Stack.push (create_task t) todo) tl
      with Exit ->
	  ()
    end
  done;
  assert (Stack.is_empty todo && Hashtbl.length running_tasks = 0);
  (*   printf "workers are@."; *)
  (*   List.iter (fun w -> printf "  %a@." print_worker w) !workers *)
  ()

let is_worker = 
  try ignore (Sys.getenv "WORKER"); true with Not_found -> false 

(*******

type 'a marshaller = {
  marshal_to : 'a -> string;
  marshal_from : string -> 'a;
}

let poly_marshaller = {
  marshal_to = (fun x -> Marshal.to_string x []);
  marshal_from = (fun s -> Marshal.from_string s 0);
}

let run_worker mres =
  let r = Worker.compute ~stop:true () in
  dprintf "worker: result is %S@." r;
  mres.marshal_from r

(* marshal and send result to all workers *)
let send_result mres r =
  let res = mres.marshal_to r in
  List.iter
    (fun w -> Protocol.Master.send w.fdout (Protocol.Master.Stop res))
    !workers;
  r

let marshal_wrapper ma mb f s =
  let x : 'a = ma.marshal_from s in
  mb.marshal_to (f x)

let marshal_wrapper2 ma mb mc f s1 s2 =
  let x : 'a = ma.marshal_from s1 in
  let y : 'b = mb.marshal_from s2 in
  mc.marshal_to (f x y)

(** Polymorphic functions. *)

let generic_map
  (ma : 'a marshaller) (mb : 'b marshaller) (mres : 'b list marshaller)
  ~(f : 'a -> 'b) (l : 'a list) : 'b list 
=
  if is_worker then begin
    Worker.register_computation "f" (marshal_wrapper ma mb f);
    (run_worker mres : 'b list)
  end else begin
    let tasks = 
      let i = ref 0 in 
      List.map (fun x -> incr i; !i, "f", ma.marshal_to x) l 
    in
    let results = Hashtbl.create 17 in (* index -> 'b *)
    master 
      ~handle:(fun (i,_,_) r -> 
		 let r = mb.marshal_from r in Hashtbl.add results i r; [])
      tasks;
    let r = List.map (fun (i,_,_) -> Hashtbl.find results i) tasks in
    send_result mres r
  end

let map ~f l = generic_map poly_marshaller poly_marshaller poly_marshaller ~f l

let generic_map_local_fold 
  (ma : 'a marshaller) (mb : 'b marshaller) (mc : 'c marshaller)
  ~(map : 'a -> 'b) ~(fold : 'c -> 'b -> 'c) acc l 
=
  if is_worker then begin
    Worker.register_computation "map" (marshal_wrapper ma mb map);
    (run_worker mc : 'c)
  end else begin
    let acc = ref acc in
    master 
      ~handle:(fun _ r -> 
		 let r = mb.marshal_from r in acc := fold !acc r; [])
      (List.map (fun x -> (), "map", ma.marshal_to x) l);
    send_result mc !acc 
  end

let map_local_fold ~map ~fold acc l = 
  generic_map_local_fold poly_marshaller poly_marshaller poly_marshaller 
    ~map ~fold acc l

let uncurry f (x,y) = f x y

let generic_map_remote_fold 
  (ma : 'a marshaller) (mb : 'b marshaller) (mc : 'c marshaller)
  ~(map : 'a -> 'b) ~(fold : 'c -> 'b -> 'c) acc l 
=
  if is_worker then begin
    Worker.register_computation "map" (marshal_wrapper ma mb map);
    Worker.register_computation2 "fold" (marshal_wrapper2 mc mb mc fold);
    (run_worker mc : 'c)
  end else begin
    let acc = ref (Some (mc.marshal_to acc)) in
    let pending = Stack.create () in
    master 
      ~handle:(fun x r -> match x with
		 | _,"map",_ -> begin match !acc with
		     | None -> Stack.push r pending; []
		     | Some v -> 
			 acc := None; 
			 [(), "fold", encode_string_pair (v, r)]
		   end
		 | _,"fold",_ -> 
		     assert (!acc = None);
		     if not (Stack.is_empty pending) then
		       [(), "fold", 
			encode_string_pair (r, Stack.pop pending)]
		     else begin
		       acc := Some r;
		       []
		     end
		 | _ -> 
		     assert false)
      (List.map (fun x -> (), "map", ma.marshal_to x) l);
    (* we are done; the accumulator must exist *)
    match !acc with
      | Some r -> send_result mc (mc.marshal_from r)
      | None -> assert false
  end

let map_remote_fold ~map ~fold acc l = 
  generic_map_remote_fold poly_marshaller poly_marshaller poly_marshaller 
    ~map ~fold acc l

let generic_map_fold_ac 
  (ma : 'a marshaller) (mb : 'b marshaller)
  ~(map : 'a -> 'b) ~(fold : 'b -> 'b -> 'b) acc l 
=
  if is_worker then begin
    Worker.register_computation "map" (marshal_wrapper ma mb map);
    Worker.register_computation2 "fold" (marshal_wrapper2 mb mb mb fold);
    (run_worker mb : 'b)
  end else begin
    let acc = ref (Some (mb.marshal_to acc)) in
    master 
      ~handle:(fun x r -> 
		 match !acc with
		 | None -> 
		     acc := Some r; []
		 | Some v -> 
		     acc := None; 
		     [(), "fold", encode_string_pair (v, r)])
      (List.map (fun x -> (), "map", ma.marshal_to x) l);
    (* we are done; the accumulator must exist *)
    match !acc with
      | Some r -> send_result mb (mb.marshal_from r)
      | None -> assert false
  end

let map_fold_ac ~map ~fold acc l = 
  generic_map_fold_ac poly_marshaller poly_marshaller 
    ~map ~fold acc l

let generic_map_fold_a 
  (ma : 'a marshaller) (mb : 'b marshaller)
  ~(map : 'a -> 'b) ~(fold : 'b -> 'b -> 'b) acc l 
=
  if is_worker then begin
    Worker.register_computation "map" (marshal_wrapper ma mb map);
    Worker.register_computation2 "fold" (marshal_wrapper2 mb mb mb fold);
    (run_worker mb : 'b)
  end else begin
    let tasks = 
      let i = ref 0 in 
      List.map (fun x -> incr i; (!i, !i), "map", ma.marshal_to x) l 
    in
    (* results maps i and j to (i,j,r) for each completed reduction of
       the interval i..j with result r *)
    let results = Hashtbl.create 17 in 
    let merge i j r = 
      if Hashtbl.mem results (i-1) then begin
	let l, h, x = Hashtbl.find results (i-1) in
	assert (h = i-1);
	Hashtbl.remove results l; 
	Hashtbl.remove results h;
	[(l, j), "fold", encode_string_pair (x, r)]
      end else if Hashtbl.mem results (j+1) then begin
	let l, h, x = Hashtbl.find results (j+1) in
	assert (l = j+1);
	Hashtbl.remove results h; 
	Hashtbl.remove results l;
	[(i, h), "fold", encode_string_pair (r, x)]
      end else begin
	Hashtbl.add results i (i,j,r);
	Hashtbl.add results j (i,j,r);
	[]
      end
    in
    master 
      ~handle:(fun x r -> match x with
		 | (i, _), "map", _ -> merge i i r
		 | (i, j), "fold", _ -> merge i j r
		 | _ -> assert false)
      tasks;
    (* we are done; results must contain 2 mappings only, for 1 and n *)
    let res = 
      try let _,_,r = Hashtbl.find results 1 in mb.marshal_from r 
      with Not_found -> acc
    in
    send_result mb res
  end

let map_fold_a ~map ~fold acc l = 
  generic_map_fold_a poly_marshaller poly_marshaller 
    ~map ~fold acc l

(** Monomorphic functions. *)

let id_marshaller = {
  marshal_to = (fun x -> x);
  marshal_from = (fun x -> x);
}

module Str = struct

  let encode_string_list l =
    let buf = Buffer.create 1024 in
    Binary.buf_string_list buf l;
    Buffer.contents buf

  let decode_string_list s =
    let l, _ = Binary.get_string_list s 0 in 
    l

  let string_list_marshaller = {
    marshal_to = encode_string_list;
    marshal_from = decode_string_list;
  }

  let map ~f l = 
    generic_map id_marshaller id_marshaller string_list_marshaller ~f l

  let encode_string s =
    let buf = Buffer.create 1024 in
    Binary.buf_string buf s;
    Buffer.contents buf

  let decode_string s =
    let s, _ = Binary.get_string s 0 in 
    s

  let string_marshaller = {
    marshal_to = encode_string;
    marshal_from = decode_string;
  }

  let map_local_fold ~map ~fold acc l =
    generic_map_local_fold id_marshaller id_marshaller string_marshaller
      ~map ~fold acc l

  let map_remote_fold ~map ~fold acc l =
    generic_map_remote_fold 
      string_marshaller string_marshaller string_marshaller
      ~map ~fold acc l

  let map_fold_ac ~map ~fold acc l =
    generic_map_fold_ac
      string_marshaller string_marshaller
      ~map ~fold acc l

  let map_fold_a ~map ~fold acc l =
    generic_map_fold_a string_marshaller string_marshaller
      ~map ~fold acc l

end

(** Master *)

module Master = struct

  let map (l : string list) : string list =
    let tasks = let i = ref 0 in List.map (fun x -> incr i; !i, "f", x) l in
    let results = Hashtbl.create 17 in (* index -> 'b *)
    master 
      ~handle:(fun (i,_,_) r -> Hashtbl.add results i r; [])
      tasks;
    List.map (fun (i,_,_) -> Hashtbl.find results i) tasks

  let map_local_fold ~(fold : 'c -> 'b -> 'c) acc l =
    let acc = ref acc in
    master 
      ~handle:(fun _ r -> acc := fold !acc r; [])
      (List.map (fun x -> (), "map", x) l);
    !acc 

  let map_remote_fold acc l =
    let acc = ref (Some acc) in
    let pending = Stack.create () in
    master 
      ~handle:(fun (_,f,_) r -> match f with
		 | "map" -> begin match !acc with
		     | None -> Stack.push r pending; []
		     | Some v -> 
			 acc := None; 
			 [(), "fold", encode_string_pair (v, r)]
		   end
		 | "fold" -> begin match !acc with
		     | None -> 
			 if not (Stack.is_empty pending) then
			   [(), "fold", 
			    encode_string_pair (r, Stack.pop pending)]
			 else begin
			   acc := Some r;
			   []
			 end
		     | Some _ -> 
			 assert false
		   end
		 | _ ->
		     assert false)
      (List.map (fun x -> (), "map", x) l);
    (* we are done; the accumulator must exist *)
    match !acc with
      | Some r -> r
      | None -> assert false

  let map_fold_ac acc l =
    let acc = ref (Some acc) in
    master 
      ~handle:(fun _ r -> match !acc with
		 | None -> 
		     acc := Some r; []
		 | Some v -> 
		     acc := None; 
		     [(), "fold", encode_string_pair (v, r)])
      (List.map (fun x -> (), "map", x) l);
    (* we are done; the accumulator must exist *)
    match !acc with
      | Some r -> r
      | None -> assert false

  let map_fold_a acc l =
    let tasks = let i = ref 0 in List.map (fun x -> incr i; !i,x) l in
    (* results maps i and j to (i,j,r) for each completed reduction of
       the interval i..j with result r *)
    let results = Hashtbl.create 17 in 
    let merge i j r = 
      if Hashtbl.mem results (i-1) then begin
	let l, h, x = Hashtbl.find results (i-1) in
	assert (h = i-1);
	Hashtbl.remove results l; 
	Hashtbl.remove results h;
	[(l, i), "fold", encode_string_pair (x, r)]
      end else if Hashtbl.mem results (j+1) then begin
	let l, h, x = Hashtbl.find results (j+1) in
	assert (l = j+1);
	Hashtbl.remove results h; 
	Hashtbl.remove results l;
	[(i, h), "fold", encode_string_pair (r, x)]
      end else begin
	Hashtbl.add results i (i,j,r);
	Hashtbl.add results j (i,j,r);
	[]
      end
    in
    master 
      ~handle:(fun x r -> match x with
		 | (i, _), "map", _ -> merge i i r
		 | (i, j), "fold", _ -> merge i j r
		 | _ -> assert false)
      (List.map (fun (i,x) -> (i,i), "map", x) tasks);
    (* we are done; results must contain 2 mappings only, for 1 and n *)
  try let _,_,r = Hashtbl.find results 1 in r with Not_found -> acc

end

*********)
