
open Format
open Control
open Network

let () = 
  if is_worker then begin
    dprintf "starting worker loop...@.";
    let compute f x = 
      let f = (Marshal.from_string f 0 : 'a -> 'b) in
      let x = (Marshal.from_string x 0 : 'a) in
      Marshal.to_string (f x) []
    in
    ignore (Worker.compute compute ~stop:false ())
  end

let master  
    ~(f : 'a -> 'b) 
    ~(handle : 'a * 'c -> 'b -> ('a * 'c) list) 
    tasks
=
  let f_closure = Marshal.to_string f [Marshal.Closures] in
  let assign_job x = f_closure, Marshal.to_string x [] in
  let handle ac r = handle ac (Marshal.from_string r 0) in
  main_master ~assign_job ~handle tasks

include Map_fold.Make(struct let master = master end)

