
open Format
open Mapreduce.Network

let worker = ref false
let mono = ref false
let test = ref ""
let local = ref false
let stop = ref false

let () = Arg.parse
  ["-w", Arg.Set worker, "runs as a worker";
   "-mono", Arg.Set mono, "use Network.Mono";
   "-test", Arg.Set_string test, "<int> ";
   "-local", Arg.Set local, "use a worker on localhost";
   "-stop", Arg.Set stop, "stop the worker";
  ]
  (fun _ -> ())
  "usage: "

let stop = !stop

let () = 
  if not !worker then begin
    if !local then 
      declare_workers ~n:2 "localhost"
    else
      declare_workers ~n:12 "moloch"
  end

let () = Mapreduce.Control.set_debug true

open Mono

let double x = x+x
let double_string x = let x = int_of_string x in string_of_int (double x)

let () =
  if !mono then begin
    if !worker then begin
      printf "I'm a mono worker...@.";
      Mono.Worker.compute double_string ()
    end else begin
      let s = ref 0 in
      Mono.Master.master 
	~handle:(fun _ r -> s := !s + int_of_string r; []) 
	(List.map (fun x -> x,()) ["1";"2";"3"]);
      printf "%d@." !s;
      assert (!s = 12)
    end
  end else begin
    if !worker then begin
      printf "I'm a poly worker...@.";
      match !test with
	| "" ->
	    Poly.Worker.compute double ~stop ()
	| "map" ->
	    Poly.Worker.map ~f:double ~stop ()
	| "map_local_fold" ->
	    Poly.Worker.map_local_fold ~map:double ~stop ()
	| "map_remote_fold" ->
	    Poly.Worker.map_remote_fold ~map:double ~fold:(+) ~stop ()
	| "map_fold_ac" ->
	    Poly.Worker.map_fold_ac ~map:double ~fold:(+) ~stop ()
	| "map_fold_a" ->
	    Poly.Worker.map_fold_a ~map:double ~fold:(+) ~stop ()
	| _ ->
	    assert false (*TODO*)
    end else begin
      match !test with
	| "" ->
	    let s = ref 0 in
	    Poly.Master.master
	      ~handle:(fun _ r -> s := !s + r; [])
	      (List.map (fun x -> x,()) [1;2;3]);
	    printf "%d@." !s;
	    assert (!s = 12)
	| "map" ->
	    assert (Poly.Master.map [1;2;3] = [2;4;6])
	| "map_local_fold" ->
	    assert (Poly.Master.map_local_fold ~fold:(+) 0 [1;2;3] = 12)
	| "map_remote_fold" ->
	    assert (Poly.Master.map_remote_fold 0 [1;2;3] = 12)
	| "map_fold_ac" ->
	    assert (Poly.Master.map_fold_ac 0 [1;2;3] = 12)
	| "map_fold_a" ->
	    assert (Poly.Master.map_fold_a 0 [1;2;3] = 12)
	| _ ->
	    assert false (*TODO*)
    end
  end


