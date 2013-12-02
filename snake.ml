
open Util

type point = int * int;;
type game_status = Playing | Dead | Starting;;
type dir_type = Left | Right | Up | Down;;

type game_elem = 
   | Body  of point list 
   | Fruit of point list 
   | Wall  of point list;;

type configuration = {
   n      : int; 
   m      : int; (* height and width *)
   dir    : dir_type; 
   status : game_status;
   elems  : game_elem list;
   speed  : float;
};;

let new_conf ?dir ?status ?snake_blocks ?speed ?elems conf = {
   n       = conf.n;
   m       = conf.m;
   dir     = get_value conf.dir    dir;
   status  = get_value conf.status status;
   speed   = get_value conf.speed  speed;
   elems   = get_value conf.elems  elems;
};;

let init sx sy = Some {
   n      = sy; 
   m      = sy;
   dir    = Up; 
   status = Starting;
   speed  = 0.3;
   elems  = [ Body [(sx / 2, sy / 2)]; Fruit []; Wall [] ];
};;

let move_point (x, y) = function 
   | Left  -> x - 1, y
   | Right -> x + 1, y
   | Up    -> x    , y + 1
   | Down  -> x    , y - 1
;;

let move body dir = 
   (move_point (List.hd body) dir) :: body
;;

let change_dir dir chr =
   match dir, chr with
   | x, 'l' when (x <> Left ) -> (Right, 1)
   | x, 'j' when (x <> Right) -> (Left,  1)
   | x, 'i' when (x <> Down ) -> (Up,    1)
   | x, 'k' when (x <> Up   ) -> (Down,  1)
   | x,  _                    -> (x,     0)
;;

let is_dead body wall n m =
   let inwall a   = List.mem a wall in
   let out (x, y) = x < 0 || x >= n || y < 0 || y >= m in

   let hit_wall = List.exists inwall body in
   let map_out  = List.exists out body in

   let rec hit_itself = function 
      | [] -> false
      | h::t -> (List.mem h t) || hit_itself t
   in

   map_out || hit_wall || hit_itself body
;;

let start_game conf key = 
   let dir, chn = change_dir conf.dir key in
   new_conf ~dir:dir ~status:Playing conf
;;

let clock_game conf key = 
   let dir, _ = change_dir conf.dir key in
   let [Body body; fruit; Wall wall] = conf.elems in
   let nbody = drop_last (move body dir) in
   let status = if (is_dead body wall conf.n conf.m) then Dead else Playing in
   let elems  = [Body nbody; fruit; Wall wall] in
   new_conf ~dir:dir ~elems:elems ~status:status conf
;;

let make_move conf key = 
   match conf.status with 
   | Dead     -> None
   | Starting -> Some (start_game conf key)
   | Playing  -> Some (clock_game conf key) 
;;

