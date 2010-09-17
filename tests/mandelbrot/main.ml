
open Graphics
(*open Functory.Sequential*)

let max_iter = 200 (* nombre maximum d'itérations *)
let f_max_iter = float max_iter (* optim *)

(* couleur = interpolation linéaire entre le rouge (loin) et le vert (près) *)
let interpolation n =
  let f = float n /. f_max_iter in
  rgb (truncate ((1. -. f) *. 255.)) (truncate (f *. 255.)) 0

let color xc yc =
  let rec iter i x y =
    if i = max_iter then
      black
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
  let m = Array.create_matrix h w black in
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

let w = int_of_string Sys.argv.(1)
let h = w * 2 / 3
let n = int_of_string Sys.argv.(2)

let xmin = -1.1
let xmax = -0.8
let ymin =  0.2
let ymax =  0.4 

let () = open_graph (Printf.sprintf " %dx%d" w h)

let () = 
  for i = 0 to n-1 do for j = 0 to n-1 do
    let xmi = xmin +. float i *. (xmax -. xmin) /. float n in
    let xma = xmin +. float (i+1) *. (xmax -. xmin) /. float n in
    let ymi = ymin +. float j *. (ymax -. ymin) /. float n in
    let yma = ymin +. float (j+1) *. (ymax -. ymin) /. float n in
    let m = draw xmi xma ymi yma (w/n) (h/n) in
    draw_image (make_image m) (i * w / n) (j * h / n)
  done done;
  (* ignore (read_key ()) *)
  ()
