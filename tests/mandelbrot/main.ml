
open Functory.Sequential
open Functory.Cores
let () = set_number_of_cores (int_of_string Sys.argv.(3))

let max_iter = 200 (* nombre maximum d'itérations *)
let f_max_iter = float max_iter (* optim *)

(* couleur = interpolation linéaire entre le rouge (loin) et le vert (près) *)
let interpolation n =
  let f = float n /. f_max_iter in
  Graphics.rgb (truncate ((1. -. f) *. 255.)) (truncate (f *. 255.)) 0

let color xc yc =
  let rec iter i x y =
    if i = max_iter then
      Graphics.black
    else 
      let x2 = x *. x in
      let y2 = y *. y in
      if x2 +. y2 > 4. then
	interpolation i
      else
	iter (succ i) (x2 -. y2 +. xc) (2. *. x *. y +. yc)
  in
  iter 0 xc yc

let draw xmin xmax ymin ymax w h =
  let m = Array.create_matrix h w Graphics.black in
  let dx = (xmax -. xmin) /. float w in
  let dy = (ymax -. ymin) /. float h in
  for i = 0 to w - 1 do
    for j = 0 to h - 1 do
      let x = xmin +. float i *. dx in
      let y = ymin +. float j *. dy in
      m.(h-1-j).(i) <- color x y
    done
  done;
  m

let width = int_of_string Sys.argv.(1)
let height = width * 2 / 3
let n = int_of_string Sys.argv.(2)

let xmin = -1.1
let xmax = -0.8
let ymin =  0.2
let ymax =  0.4 

let tasks = 
  let t = ref [] in
  for i = 0 to n-1 do for j = 0 to n-1 do
    let xmi = xmin +. float i *. (xmax -. xmin) /. float n in
    let xma = xmin +. float (i+1) *. (xmax -. xmin) /. float n in
    let ymi = ymin +. float j *. (ymax -. ymin) /. float n in
    let yma = ymin +. float (j+1) *. (ymax -. ymin) /. float n in
    t := ((xmi, xma, ymi, yma, width / n, height / n), (i,j)) :: !t
  done done;
  !t

let worker (xmi, xma, ymi, yma, w, h) = draw xmi xma ymi yma w h

let images = Array.create_matrix n n [||]

let master ((_,_,_,_,w,h), (i,j)) m = images.(i).(j) <- m; [] 

(* let og = ref false *)

(* let master ((_,_,_,_,w,h), (i,j)) m = *)
(*   if not !og then begin  *)
(*     Graphics.open_graph (Printf.sprintf " %dx%d" width height); og := true  *)
(*   end; *)
(*   let img = Graphics.make_image m in *)
(*   Graphics.draw_image img (i * w) (j * h); *)
(*   [] *)
(* Ocaml BUG? *)

let () = compute ~worker ~master tasks; ignore (Graphics.read_key ())

(* let () = *)
(*   Graphics.open_graph (Printf.sprintf " %dx%d" width height); *)
(*   for i = 0 to n-1 do for j = 0 to n-1 do *)
(*     let img = Graphics.make_image images.(i).(j) in *)
(*     Graphics.draw_image img (i * width / n) (j * height / n); *)
(*   done done; *)
(*   ignore (Graphics.read_key ()); *)
(*   () *)

(*
run on moloch

# cores  # tasks timing
1        1       3.319
2        4       1.846
        16       1.720
4        4       1.853   --> we don't control scheduling and the system
                             uses 2 processors only
        16       0.918

8      
*)


(*
Local Variables: 
compile-command: "make -C ../.. tests/mandelbrot/a.out"
End: 
*)
