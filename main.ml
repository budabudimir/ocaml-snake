
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
   fill_rect (abspos_x x) (abspos_y y) block_size_x block_size_y;;

let draw conf = 
   let draw_list clr l =
      List.iter (fun (x, y) -> drect x y clr) l
   in
   let draw_elem = function 
      | Body l -> draw_list black l
      | Fruit (Some (x, y)) -> draw_list red [(x, y)]
      | Wall l -> draw_list green l
      | _ -> ()
   in List.iter (draw_elem) conf.elems
;;

let get_dir = function
   | Left -> "Left"
   | Right -> "Right"
   | Up -> "Up"
   | Down -> "Down"
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

let rec game_loop old = function
   | None -> 0
   | Some conf when conf.status <> Dead ->
      draw conf;
      delay conf.speed;
      let nxt = make_move conf (find_next ()) in
      game_loop old nxt 
   | Some _ -> 0
;;

let main () =
   open_graph (sprintf " %d %d" window_width window_height);

   let n = window_width / block_size_x in
   let m = window_height / block_size_y in 

   game_loop None (init n m);

   close_graph ();
;;

main ();;
read_key ();;
