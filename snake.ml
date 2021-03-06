
open Util
open Printf

type point       = int * int;;
type game_status = Playing | Dead | Starting;;
type dir_type    = Left | Right | Up | Down | Undef;;

type game_elem = 
   | Body  of point list 
   | Fruit of point list 
   | Wall  of point list
;;

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
   n      = sx; 
   m      = sy;
   dir    = Undef; 
   status = Starting;
   speed  = 0.25;
   elems  = [ Body [(sx / 2, sy / 2)]; Fruit []; Wall [] ];
};;

let move_point (x, y) = function 
   | Left  -> x - 1, y
   | Right -> x + 1, y
   | Up    -> x    , y + 1
   | Down  -> x    , y - 1
   | Undef -> x    , y
;;

let move body dir = 
   (move_point (List.hd body) dir) :: body
;;

let change_dir dir ndir =
   match dir, ndir with
   | x, Right when (x <> Left ) -> Right
   | x, Left  when (x <> Right) -> Left
   | x, Up    when (x <> Down ) -> Up
   | x, Down  when (x <> Up   ) -> Down
   | x, _                       -> x 
;;

let is_dead body wall conf =
   let n = conf.n in
   let m = conf.m in
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

let gen_fruit conf = 
   (Random.int conf.n, Random.int conf.m)
;;

let eaten fruit body conf =
   let nf = diff fruit [List.hd body] in
   let et = List.mem (List.hd body) fruit in
   (et, if et || (List.length nf = 0) then (gen_fruit conf) :: nf else nf)
;;

let start_game conf ndir = 
   let dir = change_dir conf.dir ndir in
   new_conf ~dir:dir ~status:Playing conf
;;

let clock_game conf key = 
   let dir = change_dir conf.dir key in
   let [ Body body; Fruit fruit; Wall wall ] = conf.elems in
   let nbody, nfruit, speed = 
      match eaten fruit body conf with 
      | true,  f -> (move body dir, f, conf.speed -. 0.025)
      | false, f -> (drop_last (move body dir), f, conf.speed) in
   let status = if is_dead body wall conf then Dead else Playing in
   let elems  = [ Body nbody; Fruit nfruit; Wall wall ] in
   new_conf ~dir:dir ~elems:elems ~speed:speed ~status:status conf
;;

let make_move conf ndir = 
   match conf.status with 
   | Dead     -> None
   | Starting -> Some (start_game conf ndir)
   | Playing  -> Some (clock_game conf ndir) 
;;

