
open Format
open Unix
open Mapreduce

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

let () = eprintf "server_addr = %s@." (string_of_inet_addr server_addr)
	
let sockaddr = ADDR_INET (server_addr, !port) 

let () = 
  Network.declare_workers sockaddr 2;
  let l = Network.map (fun _ -> "") ["10"; "12"; "15"] in
  List.iter (fun s -> printf "%s@." s) l

