(* figure mlpost *)

open Mlpost
open Point
open Path
open Command
open Num
open Num.Infix
open Color
open Box

let fill = gray 0.8

let state =
  let node s =
    let s = "\\sf{" ^ s ^ "}" in
    round_rect ~dx:zero ~fill (tex s)
  in
  let nc = node "déconnecté" in
  let alive = node "vivant" in
  let pinged = node "contacté" in
  let error = node "inatteignable" in
  let b =
    hbox ~padding:(bp 40.)
      [nc; 
       hbox ~padding:(bp 60.)
	 [alive; vbox ~padding:(bp 30.) [pinged; error]]
      ]
  in
  let arrow ?pos ?outd ?ind lab x y = 
    Helpers.box_label_arrow ?pos ?outd ?ind ~sep:(bp 3.) 
      (Picture.tex lab) (sub x b) (sub y b) in
  draw b ++ 
  arrow ~outd:(vec (dir (-40.))) ~pos:`Bottom "connection" nc alive ++ 
  arrow ~pos:`Top "\\sf ping" alive pinged ++ 
  arrow ~pos:`Right "" pinged error ++
  arrow ~pos:`Bottom "tout message~~~~~~~" error alive ++
  arrow ~outd:(vec (dir 140.)) ~pos:`Top "{\\sf pong}/tout message" pinged alive ++
  arrow ~outd:(vec (dir 140.)) ~pos:`Top "connection perdue" alive nc

let () = Metapost.emit "state" state

let master_workers ?(padding=bp 20.) w =
  let master  = round_rect ~fill:lightblue (tex "master") in
  let w = Array.map (fun s -> round_rect ~fill:lightred  (tex s)) w in
  let wl = Array.to_list w @ [tex "etc."] in
  master, w,
  hbox ~pos:`Center ~padding:(bp 70.)
    [master;
     vbox ~padding wl]

let out b dy = Point.add (east b) (Point.pt (zero, bp dy))

let pen = Pen.transform [Transform.scaled (bp 1.5)] Pen.default

let head = Arrow.head_triangle_full
let red_head = Arrow.head_triangle_full ~color:red
let blue_head = Arrow.head_triangle_full ~color:blue
let kind = 
  Arrow.add_foot ~head (Arrow.add_line (Arrow.add_head ~head Arrow.empty)) 
let red_kind = 
  Arrow.add_line ~color:red ~pen (Arrow.add_head ~head:red_head Arrow.empty)
let blue_kind = 
  Arrow.add_line ~color:blue ~pen (Arrow.add_head ~head:blue_head Arrow.empty)

let arrow ?tex ?pos x y =
  Arrow.point_to_point ?tex ?pos ~kind (segment 0.02 x y) (segment 0.98 x y)
let red_arrow ?tex ?pos x y =
  Arrow.point_to_point ?tex ?pos ~kind:red_kind (segment 0.02 x y) 
    (segment 0.98 x y) 
let blue_arrow ?tex ?pos x y =
  Arrow.point_to_point ?tex ?pos ~kind:blue_kind (segment 0.02 x y) 
    (segment 0.98 x y) 

let master_workers_1 =
  let m, w, b = master_workers [| "worker 1"; "worker 2"; "worker 3"|] in
  draw b ++ 
  iter 0 2 
    (fun i -> arrow (out (sub m b) (2. -. 2. *. float i)) 
                    (west (sub w.(i) b))) ++
  nop

let () = Metapost.emit "master_workers_1" master_workers_1

let master_workers_network =
  let m, w, b = 
    master_workers ~padding:(bp 10.)
      [| "moloch:1"; "moloch:2"; "moloch:3";
	 "orcus:1"; "orcus:2"|] 
  in
  draw b ++ 
  iter 0 4
    (fun i -> arrow (out (sub m b) (4. -. 2. *. float i)) 
                    (west (sub w.(i) b))) ++
  nop

let () = Metapost.emit "master_workers_network" master_workers_network

let master_workers_protocol msg =
  let m, w, b = master_workers [| "worker 1"; "worker 2"; "worker 3"|] in
  let pm = out (sub m b) 2. in
  let pw1 = west (sub w.(0) b) and pw2 = west (sub w.(1) b)
			       and pw3 = west (sub w.(2) b) in
  draw b ++ 
  (match msg with 
     (* Assign *)
     | "assign" -> blue_arrow ~pos:0.7 ~tex:"assign~~~~~~~~~~~~~~" pm pw1
     (* Kill *)
     | "kill" -> blue_arrow ~pos:0.7 ~tex:"kill~~~~~~~~~~~~~" pm pw1
     (* Ping *)
     | "ping" -> blue_arrow ~pos:0.7 ~tex:"ping~~~~~~~~~~~~~" pm pw1
     (* Stop *)
     | "stop" -> blue_arrow ~pos:0.7 ~tex:"stop~~~~~~~~~~~~~" pm pw1 ++
	         blue_arrow ~pos:0.7 ~tex:"stop~~~~~~~~~" pm pw2 ++
	         blue_arrow ~pos:0.7 ~tex:"stop~~~~~~~~~~~~~" pm pw3
     (* Pong *)
     | "pong" -> red_arrow ~pos:0.3 ~tex:"pong~~~~~~~~~~~~~" pw1 pm
     (* Completed *)
     | "completed" -> red_arrow ~pos:0.3 ~tex:"completed~~~~~~~~~~~~~~~" pw1 pm
     (* Aborted *)
     | "aborted" -> red_arrow ~pos:0.3 ~tex:"aborted~~~~~~~~~~~~~" pw1 pm
     (* disconnection *)
     | "disconnection" -> arrow ~tex:"\\,{\\color{red}\\LARGE X}" pw1 pm
     (* Assign 2 *)
     | "assign_2" -> blue_arrow ~pos:0.7 ~tex:"assign~~~~~~~~~~~~~~" pm pw2
     (* Completed 2 *)
     | "completed_2" -> 
	 red_arrow ~pos:0.3 ~tex:"completed~~~~~~~~~~~~~~~" pw2 pm
     | _ -> 
	 assert false)

let () = 
  List.iter
    (fun msg ->
       let f = "master_workers_" ^ msg in
       Metapost.emit f (master_workers_protocol msg))
    ["assign"; "kill"; "ping"; "stop"; "pong"; 
     "completed"; "aborted"; "disconnection"; 
     "assign_2"; "completed_2"]

(*
Local Variables:
compile-command: "mlpost -latex slides.tex  -xpdf figures.ml"
End:
*)
