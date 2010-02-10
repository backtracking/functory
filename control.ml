
let debug = ref false

let set_debug b = debug := b

let buf = Buffer.create 1024
let fmt = Format.formatter_of_buffer buf

let dprintf s =
  Format.kfprintf 
    (fun _ -> 
       if !debug then begin 
	 Format.eprintf "%s" (Buffer.contents buf); 
	 Buffer.reset buf;
	 flush stderr 
       end)
    fmt s


