open Joueur
type settings = {
  starttime : float;
  isstartvisible : bool;
  is_game_running : bool;
}
type plateforme = {
  platform_x : float;
  platform_y : float;
  platform_width : float;
  platform_height : float
}

type entities = {
player : joueur;
plateforme_list : plateforme list
}


val gameloop :  unit -> unit


(*
val draw : joueur -> ennemi list -> unit

val init : string -> string -> (string * string) list -> (joueur * ennemi list)

val startscreen  : string -> unit

val menu : string -> unit

val setup : settings -> settings

*)