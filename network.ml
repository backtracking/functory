
open Format
open Unix
open Control

module Worker = struct

  let computations : (string, (string -> string)) Hashtbl.t = Hashtbl.create 17

  let register_computation = Hashtbl.add computations
    
  type running_task = {
    pid : int;
    file : file_descr;
  }

  open Protocol

  exception ExitOnStop of string

  let server_fun cin cout =
    dprintf "new connection@.";
    let fdin = descr_of_in_channel cin in
    let fdout = descr_of_out_channel cout in
    let pids = Hashtbl.create 17 in (* ID -> running_task *)
    let handle_message_from_master _ = 
      let m = Master.receive fdin in
      dprintf "received: %a@." Master.print m;
      match m with
	| Master.Assign (id, f, a) ->
	    if Hashtbl.mem computations f then begin
	      let f = Hashtbl.find computations f in
	      Worker.send fdout (Worker.Started id);
	      let fin, fout = pipe () in
	      begin match fork () with
		| 0 -> 
		    close fin;
		    (* perform computation *)
		    dprintf "  id %d: computation is running...@." id;
		    let r = f a in
		    let c = out_channel_of_descr fout in
		    output_value c r;
		    dprintf "  id %d: computation done@." id;
		    exit 0
		| pid -> 
		    close fout;
		    let t = { pid = pid; file = fin } in
		    Hashtbl.add pids id t
	      end
	    end else
	      Worker.send fdout (Worker.Aborted id)
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
	  eprintf "anomaly: %s@." (Printexc.to_string e); 
	  exit 1

  (* sockets are allocated lazily *)
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
      bind sock sockaddr;
      listen sock 3;
      let s, _ = Unix.accept sock in 
      Hashtbl.add sockets port (sock, s);
      sock, s

  let compute ?(stop=false) ?(port=51000) () = 
    let _, s = get_socket port in
    if stop then begin
      let inchan = Unix.in_channel_of_descr s 
      and outchan = Unix.out_channel_of_descr s in 
      server_fun inchan outchan 
    end else begin
      while true do
	match Unix.fork() with
	  | 0 -> 
	      if Unix.fork() <> 0 then exit 0; 
              let inchan = Unix.in_channel_of_descr s 
              and outchan = Unix.out_channel_of_descr s in 
	      ignore (server_fun inchan outchan);
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

type worker = { 
  sockaddr : sockaddr;
  mutable connected : bool;
  mutable fdin : file_descr;
  mutable fdout : file_descr;
}

type logical_worker = {
  worker : worker;
  wid : int;
}

let workers = ref []
let logical_workers = ref []

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

let declare_workers ?(port=51000) ?(n=1) s = 
  let a = create_sock_addr s port in
  let w = { sockaddr = a; connected = false; fdin = stdin; fdout = stdout } in
  workers := w :: !workers;
  for i = 1 to n do 
    logical_workers := { worker = w; wid = i }  :: !logical_workers 
  done

let connect_worker w =
  if not w.connected then begin
    let ic,oc = open_connection w.sockaddr in
    let fdin = descr_of_in_channel ic in
    let fdout = descr_of_out_channel oc in
    w.connected <- true;
    w.fdin <- fdin;
    w.fdout <- fdout
  end

let next_id = let r = ref 0 in fun () -> incr r; !r

let print_sockaddr fmt = function
  | ADDR_UNIX s -> fprintf fmt "%s" s
  | ADDR_INET (ia, port) -> fprintf fmt "%s:%d" (string_of_inet_addr ia) port

let master 
  ~(handle : 'a * string * string -> string -> ('a * string * string) list) 
  (tasks : ('a * string * string) list)
=
  let running_tasks = Hashtbl.create 17 in
  let create_job lw ((_,f,a) as task) =
    connect_worker lw.worker;
    let id = next_id () in
    Protocol.Master.send lw.worker.fdout (Protocol.Master.Assign (id, f, a));
    Hashtbl.add running_tasks id (lw, task)
  in
  let wait continuation =
    let listen_for_worker w =
      let l,_,_ = select [w.fdin] [] [] 0.1 in
      if l = [] then raise Exit;
      let m = Protocol.Worker.receive w.fdin in
      dprintf "received from %a: %a@." print_sockaddr w.sockaddr
	Protocol.Worker.print m;
      match m with
	| Protocol.Worker.Started _ ->
	    raise Exit
	| Protocol.Worker.Completed (id, r) ->
	    let lw, task = Hashtbl.find running_tasks id in
	    Hashtbl.remove running_tasks id;
	    lw, continuation task r
	| Protocol.Worker.Aborted id ->
	    let lw, task = Hashtbl.find running_tasks id in
	    Hashtbl.remove running_tasks id;
	    lw, [task]
    in
    let rec loop = function
      | [] -> loop !workers
      | w :: wl -> try listen_for_worker w with Exit -> loop wl
    in
    loop !workers
  in
  List.iter connect_worker !workers;
  Master.run
    ~create_job
    ~wait:(fun () -> wait handle)
    !logical_workers tasks

let is_worker = 
  try ignore (Sys.getenv "WORKER"); true with Not_found -> false 

type 'a marshaller = {
  marshal_to : 'a -> string;
  marshal_from : string -> 'a;
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

let generic_map
  (ma : 'a marshaller) (mb : 'b marshaller) (mres : 'b list marshaller)
  (f : 'a -> 'b) (l : 'a list) : 'b list 
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

let poly_marshaller = {
  marshal_to = (fun x -> Marshal.to_string x []);
  marshal_from = (fun s -> Marshal.from_string s 0);
}

let map f l = generic_map poly_marshaller poly_marshaller poly_marshaller f l

(***
let map_local_reduce ~(map : 'a -> 'b) ~(reduce : 'c -> 'b -> 'c) acc l =
  if is_worker then begin
    Worker.register_computation "map" (marshal_wrapper map);
    (run_worker () : 'b list)
  end else begin
    let acc = ref acc in
    master 
      ~handle:(unmarshal_result (fun _ r -> acc := reduce !acc r; []))
      (List.map (fun x -> (), "map", x) l);
    send_result !acc 
  end
***)

(* and its instances *)

let id_marshaller = {
  marshal_to = (fun x -> x);
  marshal_from = (fun x -> x);
}

module String = struct

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

  let map f l = 
    generic_map id_marshaller id_marshaller string_list_marshaller f l

end
