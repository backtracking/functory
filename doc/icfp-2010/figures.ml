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
  let error = node "error" in
  let b =
    hbox ~padding:(bp 40.)
      [nc; alive; vbox ~padding:(bp 20.) [pinged; error]
      ]
  in
  let arrow x y = Helpers.box_arrow ~sep:(bp 3.) (sub x b) (sub y b) in
  draw b ++ 
  arrow nc alive ++ arrow alive pinged ++ 
  arrow pinged error ++ arrow error alive

let () = Metapost.emit "state" state

(*
Local Variables:
compile-command: "mlpost -latex main.tex  -xpdf figures.ml"
End:
*)
