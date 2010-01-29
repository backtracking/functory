
open Format
open Unix

let long_computation () =
  sleep 3;
  42

let process (f : 'a -> 'b) (x : 'a) : 'b =
  let file = Filename.temp_file "result" "" in
  match fork () with
    | 0 -> (* child *)
	let r = f x in
	let c = open_out file in
	output_value c r;
	close_out c;
	exit 0
    | pid -> (* parent *)
	match wait () with
	  | p, WEXITED e ->
	      assert (p = pid);
	      printf "PID %d: exit code = %d@." p e;
	      let c = open_in file in
	      let r = input_value c in
	      close_in c;
	      r
	  | p, _ ->
	      printf "PID %d: killed or stopped!@." p;
	      failwith "process"

let r : int = process long_computation ()

(* TODO *)
let map = List.map
