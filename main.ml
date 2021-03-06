
open Graphics
open Printf
open Snake
open Util

let window_width  = 650;;
let window_height = 450;;
let block_size_x  = 10;;
let block_size_y  = 10;;

let abspos_x x = x * block_size_x;;
let abspos_y y = y * block_size_y;;

let drect x y clr =
   set_color clr;
   fill_rect (abspos_x x) (abspos_y y) block_size_x block_size_y
;;

let body_color  = black;;
let fruit_color = red;;
let wall_color  = green;;

let draw conf = 
   let draw_list clr l =
      List.iter (fun (x, y) -> drect x y clr) l
   in
   let draw_elem = function 
      | Body  l -> draw_list body_color  l
      | Fruit l -> draw_list fruit_color l 
      | Wall  l -> draw_list wall_color  l
   in List.iter (draw_elem) (List.rev conf.elems)
;;

let string_of_dir = function
   | Left  -> "Left"
   | Right -> "Right"
   | Up    -> "Up"
   | Down  -> "Down"
   | Undef -> "Undef"
;;

let valid x = List.mem x ['k'; 'l'; 'i'; 'j'];;

let find_next () =
   let last = ref '\000' in
   while key_pressed () && not (valid !last) do 
      let k = read_key () in
      last := if valid k then k else !last;
   done;
   !last
;;

let del_elem (xs, ys) =
   let f lc lp = List.iter (fun (x, y) -> drect x y background) (diff lp lc) in
   match xs, ys with
   | Body  lc, Body  lp -> f lc lp
   | Fruit lc, Fruit lp -> f lc lp
   | Wall  lc, Wall  lp -> f lc lp
   | _, _               -> ()
;; 

let delete curr = function
   | Some prev -> List.iter (del_elem) (List.combine curr.elems prev.elems)
   | None      -> ()
;;

let get_dir = function
   | 'l' -> Right
   | 'i' -> Up
   | 'j' -> Left
   | 'k' -> Down
   |  _  -> Undef
;;

let rec game_loop old = function
   | None -> ()
   | Some conf when conf.status <> Dead ->
      draw conf;
      delete conf old;
      delay conf.speed;
      let nxt = make_move conf (get_dir (find_next ())) in
      game_loop (Some conf) nxt 
   | Some _ -> ()
;;

let main () =
   open_graph (sprintf " %d %d" window_width window_height);

   let n = window_width  / block_size_x in
   let m = window_height / block_size_y in 

   game_loop None (init n m);
;;

main ();;
read_key ()

