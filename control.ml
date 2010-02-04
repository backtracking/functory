
let debug = ref true

let set_debug b = debug := b

let dprintf fmt =
  Format.ksprintf 
    (fun s -> if !debug then begin Format.eprintf "%s" s; flush stderr end)
    fmt


