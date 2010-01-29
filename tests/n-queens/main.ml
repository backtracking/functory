
open Mapreduce

let rec t a b c count =
  if a > 0 then
    let rec loop e count =
      if e > 0 then
	let d = e land (-e) in
	loop (e - d) (t (a-d) ((b+d)*2) ((c+d)/2) count)
      else
	count
    in
    loop (a land lnot b land lnot c) count
  else
    count+1

let () = 
  let q = int_of_string Sys.argv.(1) in
  Format.printf "%d@." (t (lnot ((lnot 0) lsl q)) 0 0 0)

(*
Local Variables: 
compile-command: "make -C ../.. tests/n-queens/a.out"
End: 
*)


