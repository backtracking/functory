
open Format
open Unix

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
  try 
    while true do    
      let s = input_line cin in 
      printf "received %S@." s;
      Printf.fprintf cout "%s\n" s;
      flush cout
    done
  with 
    | End_of_file -> printf "End of text@."; exit 0 
    | e -> printf "anomaly: %s@." (Printexc.to_string e); exit 1

let () = establish_server serv_fun sockaddr

