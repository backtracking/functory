
open Format
open Unix
open Protocol

let server = ref "localhost"
let port = ref 51000
let () = 
  Arg.parse 
    ["-server", Arg.Set_string server, "<machine>   sets the server name";
     "-port", Arg.Set_int port, "<n>  sets the port number";
    ]
    (fun _ -> ())
    "usage"

let server_addr =
  try  
    inet_addr_of_string !server 
  with Failure "inet_addr_of_string" -> 
    try 
      (gethostbyname !server).h_addr_list.(0) 
    with Not_found ->
      eprintf "%s : Unknown server@." !server ;
      exit 2
	
let sockaddr = ADDR_INET (server_addr, !port) 

let master_test () =
  let ic,oc = open_connection sockaddr in
  let fdin = descr_of_in_channel ic in
  let fdout = descr_of_out_channel oc in
  Master.send fdout (Master.Assign (3, "hello"));
  let _,_,_ = select [fdin] [] [] (-1.) in
  let m = Worker.receive fdin in
  eprintf "received: %a@." Worker.print m;
  sleep 2;
  Master.send fdout (Master.Kill 3);
  shutdown_connection ic


