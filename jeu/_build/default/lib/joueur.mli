type deplacement =
  | HAUT
  | BAS
  | GAUCHE
  | DROITE

type position = float * float

type hitboxrectangle = position * position * position * position

type joueur = {
    nom : string;
    pos : position;
    vector_velocity : float *float;
    health_point : int;
    attack_point : int;
    jetpack_carburant_pourcentage : int;
    has_grappin : bool;
    sprite : string;
    sprite_height : float;
    sprite_width : float;
    facing_right : bool;
    is_jumping : bool
}


val deplacer : joueur -> joueur

val vel : joueur -> float * float -> joueur

val jump : joueur -> bool -> joueur

(* val modify_player : joueur -> float * float -> float * float -> int -> int -> int -> bool -> joueur *)

val create_personnage : string -> string -> float -> float -> joueur

(*val use_jetpack : joueur -> deplacement -> joueur

val use_grappin : joueur -> deplacement -> joueur

val attack : joueur -> hitbox
*)