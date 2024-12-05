open Joueur
open Ennemi

type settings

val gameloop : joueur -> ennemi list -> unit

val draw : joueur -> ennemi list -> unit

val init : string -> string -> (string * string) list -> (joueur * ennemi list)

val startscreen  : string -> unit

val menu : string -> unit

val setup : settings -> settings

