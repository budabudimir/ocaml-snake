
let delay sec = 
   let span = Sys.time () +. sec in
   while Sys.time () < span do () done
;;

let rec drop_last = function 
   | [] | [_] -> []
   | h::t -> h :: drop_last t
;;

let get_value def = function 
   | Some x -> x
   | None   -> def
;;
