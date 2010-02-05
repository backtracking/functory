
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

let serv_fun cin cout =
  printf "new connection@.";
  let fdin = descr_of_in_channel cin in
  let fdout = descr_of_out_channel cout in
  let receive_message _ = 
    let m = Master.receive fdin in
    printf "received: %a@." Master.print m;
    sleep 3;
    Worker.send fdout (Worker.Started 42)
  in
  try 
    while true do    
      let l,_,_ = select [fdin] [] [] 1. in
      List.iter receive_message l
    done
  with 
    | End_of_file -> printf "End of text@."; exit 0 
    | e -> printf "anomaly: %s@." (Printexc.to_string e); exit 1

let () = establish_server serv_fun sockaddr

