
open Functory.Network
let () = declare_workers ~n:8 "belzebuth"
let () = Functory.Control.set_debug true
open Poly

let is_worker = Array.length Sys.argv >= 2 && Sys.argv.(1) = "-w"

let max_iter = 200 (* nombre maximum d'itérations *)
let f_max_iter = float max_iter 

type quad =
  | White
  | Black
  | Quad of quad * quad * quad * quad

let quad = function
  | White, White, White, White -> White
  | Black, Black, Black, Black -> Black
  | q1, q2, q3, q4 -> Quad (q1, q2, q3, q4)

let color xc yc =
  let rec iter i x y =
    if i = max_iter then
      Black
    else 
      let x2 = x *. x in
      let y2 = y *. y in
      if x2 +. y2 > 4. then
	White
      else
	iter (succ i) (x2 -. y2 +. xc) (2. *. x *. y +. yc)
  in
  iter 0 xc yc

let rec draw x y w n =
  if n = 0 then
    color x y
  else
    let w' = w /. 2. in
    let x' = x +. w' in
    let y' = y +. w' in
    let n' = n - 1 in
    quad (draw x y w' n', draw x' y w' n', draw x y' w' n', draw x' y' w' n')

let worker (x, y, w, n) = draw x y w n

let () = if is_worker then begin Worker.compute worker (); assert false end

let width = int_of_string Sys.argv.(1)
let height = width
let t = int_of_string Sys.argv.(2)

let rec log2 x = if x = 1 then 0 else 1 + log2 (x lsr 1)
let () = assert (width mod t = 0)
let n = log2 (width / t)
let () = assert (width / t = 1 lsl n)

let xmin = -2.0
let xmax =  1.0
let ymin = -1.5
let ymax =  1.5 

let () = assert (xmax -. xmin = ymax -. ymin)

let tasks = 
  let l = ref [] in
  for j = 0 to t-1 do
    let ymi = ymin +. float j *. (ymax -. ymin) /. float t in
    let yma = ymin +. float (j+1) *. (ymax -. ymin) /. float t in
    l := ((xmin, xmax, ymi, yma, width, height / t), j) :: !l
  done;
  !l

let images = Array.create t ([||] : Graphics.color array array)

let master (_, j) m = images.(j) <- m; []

let () = Master.compute ~master tasks

let () = 
  if false then begin
    Graphics.open_graph (Printf.sprintf " %dx%d" width height);
    let h = height / t in
    Array.iteri
      (fun j m ->
	 let img = Graphics.make_image m in
	 Graphics.draw_image img 0 (j * h))
      images; 
    ignore (Graphics.read_key ())
  end


(*
Local Variables: 
compile-command: "make -C ../.. tests/mandelbrot/quad_poly.opt"
End: 
*)
