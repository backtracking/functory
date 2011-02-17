
open Functory.Network
let () = declare_workers ~n:8 "belzebuth"
let () = Functory.Control.set_debug false
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

let xmin = ref (-2.0)
let xmax = ref ( 1.0)
let ymin = ref (-1.5)
let ymax = ref ( 1.5)

let () = assert (!xmax -. !xmin = !ymax -. !ymin)

let tasks () = 
  let l = ref [] in
  let w = (!xmax -. !xmin) /. float t in
  for i = 0 to t-1 do for j = 0 to t-1 do
    let x = !xmin +. float i *. w in
    let y = !ymin +. float j *. w in
    l := ((x, y, w, n), (i, j)) :: !l
  done done;
  !l

let locale = GtkMain.Main.init ()
let window = GWindow.window ()
let vbox = GPack.vbox ~packing:window#add ()
let button = GButton.button ~label:"Start" ~packing:(vbox#pack ~padding:5) ()
let canvas = GnoCanvas.canvas ~width ~height ~packing:vbox#pack ()
let () = canvas#set_scroll_region 0. 0. (float width) (float height)
let group = GnoCanvas.group canvas#root ~x:0. ~y:0.

let fill_rect x y w h =
  ignore 
    (GnoCanvas.rect group
       ~props:[ `X1 (float x); `Y1 (float y); 
		`X2 (float (x + w)); `Y2 (float (y + h)); 
		`FILL_COLOR "black" ])

let rec draw_quad x y w = function
  | White -> ()
  | Black -> fill_rect x y w w
  | Quad (q1, q2, q3, q4) ->
      let w = w / 2 in
      draw_quad x y w q1;
      draw_quad (x + w) y w q2;
      draw_quad x (y + w) w q3;
      draw_quad (x + w) (y + w) w q4

let master (_, (i,j)) q = 
  let w = width / t in
  draw_quad (i*w) (j*w) w q; []

let c = Master.create_computation ~master (tasks ())
let callback () = Master.one_step c; not (Master.is_done c)

let _ = 
  let rect = GnoCanvas.rect group
    ~props:[ `X1 0.; `Y1 0.; 
	     `X2 (float width); `Y2 (float height);
	     `FILL_COLOR "tan"]
  in
  rect#connect#event 
    ~callback:(fun ev -> begin match ev with
		 | `BUTTON_PRESS ev ->
		     let x = GdkEvent.Button.x ev  in
		     let y = GdkEvent.Button.y ev  in
		     Format.printf "click at %f, %f@." x y
		 | _ -> ()
	       end;
	      false)

let _ =
  button#connect#clicked ~callback:
    (fun () -> ignore (GMain.Timeout.add ~ms:10 ~callback))

let () = 
  ignore (window#connect#destroy ~callback:GMain.Main.quit);
  window#show () ;
  GMain.Main.main ()

(*
Local Variables: 
compile-command: "make -C ../.. tests/mandelbrot/gquad.gtk2"
End: 
*)
