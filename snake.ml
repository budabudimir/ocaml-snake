
open Graphics

type point = int * int;;
type game_status = Playing | Dead | Starting;;
type block_type = Wall | Body | Fruit;;
type direction_type = Left | Right | Up | Down;;

type configuration = {
   n : int; m : int; (* height and width *)
   direction : direction_type; 
   status : game_status;
   fruit : point option;
   snake_blocks : point list;
   speed : float;
};;

let get_value def = function
   | Some x -> x 
   | None   -> def
;;

let new_conf ?dir ?status ?fruit ?snake_blocks ?speed conf = {
   n = conf.n;
   m = conf.m;
   direction    = get_value conf.direction    dir;
   status       = get_value conf.status       status;
   fruit        = get_value conf.fruit        fruit;
   snake_blocks = get_value conf.snake_blocks snake_blocks;
   speed        = get_value conf.speed        speed;
};;

let init sx sy = Some {
   n = sy; 
   m = sy;
   direction = Up; 
   status = Starting;
   fruit = None;
   snake_blocks = [(sx / 2, sy / 2)];
   speed = 0.5;
};;

let change_dir dir chr =
   match dir, chr with
   | x, 'l' when (x <> Left ) -> (Right, 1)
   | x, 'j' when (x <> Right) -> (Left,  1)
   | x, 'i' when (x <> Down ) -> (Up,    1)
   | x, 'k' when (x <> Up   ) -> (Down,  1)
   | x,  _                    -> (x,     0)
;;

let start_game conf evnt = 
   let dir, chn = change_dir conf.direction evnt.key in
   new_conf ~dir:dir ~status:Playing conf
;;

let make_move conf evnt = 
   match conf.status with 
   | Dead     -> None
   | Starting -> Some (start_game conf evnt)
   | Playing  -> None 
;;

