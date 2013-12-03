
let delay sec = 
   let span = Sys.time () +. sec in
   while Sys.time () < span do () done
;;

let rec drop_last = function 
   | [] | [_] -> []
   | h::t -> h :: drop_last t
;;

let rec take_last = function 
   | []   -> None
   | [l]  -> Some l
   | h::t -> take_last t
;;

(* add e to the last position in list l *)
let (/:) e l =
   let rec f = function
      | [] -> [e]
      | h::t -> h :: f t
   in f l
;;

let get_value def = function 
   | Some x -> x
   | None   -> def
;;

let diff x y =
   let p e = not (List.mem e y) in
   List.filter p x
;;
