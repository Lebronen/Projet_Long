type deplacement =
  | HAUT
  | BAS
  | GAUCHE
  | DROITE

type position = int *int

type hitbox = position * position * position * position

type joueur = {
    nom : string;
    pos : position;
    health_point : int;
    attack_point : int;
    jetpack_carburant_pourcentage : int;
    has_grappin : bool;
    sprite_img_name : string

}


val deplacer : joueur -> deplacement -> joueur

(*val drawme : joueur -> unit*)

val create_personnage : string -> string -> joueur

(*val use_jetpack : joueur -> deplacement -> joueur

val use_grappin : joueur -> deplacement -> joueur

val attack : joueur -> hitbox
*)