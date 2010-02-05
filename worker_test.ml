
open Format
open Unix
open Mapreduce.Protocol

let port = ref 51000
let () = 
  Arg.parse
    ["-port", Arg.Set_int port, "<n>  sets the port number"]
    (fun _ -> ())
    "worker_test: usage:"

let my_address =
  (Unix.gethostbyname (Unix.gethostname ())).Unix.h_addr_list.(0)

let sockaddr = 
  Unix.ADDR_INET (my_address, !port)

let compute s = s ^ s

type running_task = {
  pid : int;
  file : file_descr;
}

let serv_fun cin cout =
  printf "new connection@.";
  let fdin = descr_of_in_channel cin in
  let fdout = descr_of_out_channel cout in
  let pids = Hashtbl.create 17 in (* ID -> running_task *)
  let handle_message_from_master _ = 
    let m = Master.receive fdin in
    printf "received: %a@." Master.print m;
    match m with
      | Master.Assign (id, s) ->
	  Worker.send fdout (Worker.Started id);
	  let fin, fout = pipe () in
	  begin match fork () with
	    | 0 -> 
		close fin;
		(* perform computation *)
		eprintf "  id %d: computation is running...@." id;
		let r = compute s in
		let c = out_channel_of_descr fout in
		output_value c r;
		eprintf "  id %d: computation done@." id;
		exit 0
	    | pid -> 
		close fout;
		let t = { pid = pid; file = fin } in
		Hashtbl.add pids id t
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
	  Worker.send fdout (Worker.Aborted id)
  in
  try 
    while true do    
      let l,_,_ = select [fdin] [] [] 1. in
      List.iter handle_message_from_master l;
      Hashtbl.iter wait_for_completed_task pids
    done
  with 
    | End_of_file -> 
	printf "master disconnected@."; 
	Hashtbl.iter (fun _ t -> kill t.pid Sys.sigkill) pids;
	exit 0 
    | e -> 
	printf "anomaly: %s@." (Printexc.to_string e); exit 1

let () = establish_server serv_fun sockaddr

