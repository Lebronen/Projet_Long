
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
player : Joueur.t;
ennemis : ennemi;
plateforme_list : plateforme list
}


type direction = Below | Left | Right


val gameloop :  unit -> unit

val check_plateforme : direction -> (float * float) -> (float * float) -> float -> float -> plateforme -> bool

val is_on_plateforme : direction -> Joueur.t -> plateforme list -> bool
val is_on_plateforme_ennemi : direction -> ennemi -> plateforme list -> bool

val wich_plateforme : direction -> Joueur.t -> plateforme list -> plateforme list
val wich_plateforme_ennemi : direction -> ennemi -> plateforme list -> plateforme list


(*
val draw : joueur -> ennemi list -> unit

val init : string -> string -> (string * string) list -> (joueur * ennemi list)

val startscreen  : string -> unit

val menu : string -> unit

val setup : settings -> settings

*)