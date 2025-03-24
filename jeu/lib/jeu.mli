open Joueur
open Ennemi

type settings = {
  starttime : float;
  isstartvisible : bool;
  is_game_running : bool;
}
type plateforme = {
  platform_x : int;
  platform_y : int;
  platform_width : int;
  platform_height : int
}

type entities = {
player : joueur;
ennemis : ennemi list;
plateforme_list : plateforme list
}


val gameloop :  unit -> unit

val check_plateforme : Joueur.joueur -> plateforme -> bool

val is_on_plateforme : Joueur.joueur -> plateforme list -> bool

val wich_plateforme : Joueur.joueur -> plateforme list -> plateforme list

(*
val draw : joueur -> ennemi list -> unit

val init : string -> string -> (string * string) list -> (joueur * ennemi list)

val startscreen  : string -> unit

val menu : string -> unit

val setup : settings -> settings

*)