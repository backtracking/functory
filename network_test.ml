
open Format
open Mapreduce.Network

let worker = ref false
let mono = ref false

let () = Arg.parse
  ["-w", Arg.Set worker, "runs as a worker";
   "-mono", Arg.Set mono, "use Network.Mono";]
  (fun _ -> ())
  "usage: "

let () = declare_workers ~n:12 "moloch"
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
      Poly.Worker.compute double ()
    end else begin
      let s = ref 0 in
      Poly.Master.master 
	~handle:(fun _ r -> s := !s + r; []) 
	(List.map (fun x -> x,()) [1;2;3]);
      printf "%d@." !s;
      assert (!s = 12)
    end
  end



