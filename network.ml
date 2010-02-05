
open Format
open Unix


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

let declare_workers a n = 
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

let running_tasks = Hashtbl.create 17

let create_job lw ((_,s) as task) =
  connect_worker lw.worker;
  let id = next_id () in
  Protocol.Master.send lw.worker.fdout (Protocol.Master.Assign (id, s));
  Hashtbl.add running_tasks id (lw, task)

let print_sockaddr fmt = function
  | ADDR_UNIX s -> fprintf fmt "%s" s
  | ADDR_INET (ia, port) -> fprintf fmt "%s:%d" (string_of_inet_addr ia) port

let wait continuation =
  let listen_for_worker w =
    let l,_,_ = select [w.fdin] [] [] 0.1 in
    if l = [] then raise Exit;
    let m = Protocol.Worker.receive w.fdin in
    eprintf "received from %a: %a@." print_sockaddr w.sockaddr
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

let master ~(f : 'a -> 'b) ~(handle : 'a -> 'b -> 'a list) tasks =
  Master.run
    ~create_job
    ~wait:(fun () -> wait handle)
    !logical_workers tasks


(* and its instances *)

let map f l =
  let tasks = let i = ref 0 in List.map (fun x -> incr i; !i,x) l in
  let results = Hashtbl.create 17 in (* index -> 'b *)
  master 
    ~f:(fun (_,x) -> f x)
    ~handle:(fun (i,_) r -> Hashtbl.add results i r; [])
    tasks;
  List.map (fun (i,_) -> Hashtbl.find results i) tasks



(**** test **************************************************)

(****
let master_test () =
  let ic,oc = open_connection sockaddr in
  at_exit (fun () -> shutdown_connection ic);
  let fdin = descr_of_in_channel ic in
  let fdout = descr_of_out_channel oc in
  let id = ref 0 in
  while true do
    incr id;
    let msg = "hello " ^ string_of_int (Random.int 1000) in
    Protocol.Master.send fdout (Protocol.Master.Assign (!id, msg));
    let l,_,_ = select [fdin] [] [] 1. in
    List.iter
      (fun _ -> 
	 let m = Protocol.Worker.receive fdin in
	 eprintf "received: %a@." Protocol.Worker.print m) l;
  done;
(*   Master.send fdout (Master.Kill 3); *)
(*    *)
  ()



****)
