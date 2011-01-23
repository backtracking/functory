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

let head = Arrow.head_triangle_full
let kind = 
    Arrow.add_foot ~head (Arrow.add_line (Arrow.add_head ~head Arrow.empty)) 

let arrow ?tex ?pos x y =
  Arrow.point_to_point ?tex ?pos ~kind (segment 0.02 x y) (segment 0.98 x y)

let master_workers_1 =
  let m, w, b = master_workers [| "worker 1"; "worker 2"; "worker 3"|] in
  draw b ++ 
  iter 0 2 
    (fun i -> arrow (out (sub m b) (2. -. 2. *. float i)) 
                    (west (sub w.(i) b))) ++
  nop

let () = Metapost.emit "master_workers_1" master_workers_1

let master_workers_2 =
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

let () = Metapost.emit "master_workers_2" master_workers_2

(*
Local Variables:
compile-command: "mlpost -latex slides.tex  -xpdf figures.ml"
End:
*)
