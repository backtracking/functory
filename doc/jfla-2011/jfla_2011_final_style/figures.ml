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

(*
Local Variables:
compile-command: "mlpost -latex main2.tex  -xpdf figures.ml"
End:
*)
