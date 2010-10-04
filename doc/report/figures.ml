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
  let nc = node "not connected" in
  let alive = node "alive" in
  let pinged = node "pinged" in
  let error = node "unreachable" in
  let b =
    hbox ~padding:(bp 40.)
      [nc; 
       hbox ~padding:(bp 50.)
	 [alive; vbox ~padding:(bp 30.) [pinged; error]]
      ]
  in
  let arrow ?pos ?outd ?ind lab x y = 
    Helpers.box_label_arrow ?pos ?outd ?ind ~sep:(bp 3.) 
      (Picture.tex lab) (sub x b) (sub y b) in
  draw b ++ 
  arrow ~outd:(vec (dir (-40.))) ~pos:`Bottom "connect" nc alive ++ 
  arrow ~pos:`Top "\\sf ping" alive pinged ++ 
  arrow ~pos:`Right "" pinged error ++
  arrow ~pos:`Bottom "any msg.~~~~~" error alive ++
  arrow ~outd:(vec (dir 140.)) ~pos:`Top "{\\sf pong}/any msg." pinged alive ++
  arrow ~outd:(vec (dir 140.)) ~pos:`Top "lost connection" alive nc

let () = Metapost.emit "state" state

(*
Local Variables:
compile-command: "mlpost -latex main2.tex  -xpdf figures.ml"
End:
*)
