
open Format
open Unix

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
  output_string oc "hello\n"; flush oc;
  sleep 3;
  output_string oc "bye\n"; flush oc;
  shutdown_connection ic


