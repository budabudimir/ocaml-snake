
open Thread
open Graphics
open Printf
open Snake

let window_width  = 650;;
let window_height = 450;;
let block_size_x  = 10;;
let block_size_y  = 10;;

let abspos_x x = x * block_size_x;;
let abspos_y y = y * block_size_y;;

let delay sec = 
   let span = Sys.time () +. sec in
   while Sys.time () < span do () done;;

let drect x y clr =
   set_color clr;
   fill_rect (abspos_x x) (abspos_y y) block_size_x block_size_y;;

let draw conf = 
   let rec draw_snake = function 
      | [] -> ()
      | (x,y)::t -> 
            drect x y black;
            draw_snake t
   in draw_snake conf.snake_blocks;

   let draw_fruit = function
      | Some (x, y) -> drect x y red 
      | None -> ()
   in draw_fruit conf.fruit
;;

let rec game_loop = function
   | None -> 0
   | Some conf ->
      draw conf;
      delay conf.speed;
      let nxt = make_move conf (wait_next_event [Poll]) in
      game_loop nxt
;;


let main () =
   open_graph (sprintf " %d %d" window_width window_height);

   let n = window_width / block_size_x in
   let m = window_height / block_size_y in 

   game_loop (init n m)
;;

main ();;
read_key ();;
