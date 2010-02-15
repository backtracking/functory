
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


let (++) = Int64.add

let () = Network.declare_workers ~n:4 "moloch"

let () =
  let l = 
    Network.Master.map ["10"; "20"; "15"] 
  in
  let v = List.fold_left (fun acc s -> acc ++ Int64.of_string s) 0L l in
  printf "total = %Ld@." v

let () =
  let s = 
    Network.Master.map_local_reduce ~reduce:(^) "" ["10"; "20"; "15"] 
  in
  printf "s = %s@." s

let () =
  let s = 
    Network.Master.map_remote_reduce "" ["10"; "20"; "15"] 
  in
  printf "s = %s@." s

let () =
  let s = 
    Network.Master.map_reduce_a "" ["10"; "20"; "15"]
  in
  printf "s = %s@." s

