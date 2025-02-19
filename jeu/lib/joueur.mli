type deplacement =
  | HAUT
  | BAS
  | GAUCHE
  | DROITE

type position = int *int

type hitboxrectangle = position * position * position * position

type joueur = {
    nom : string;
    pos : position;
    vector_velocity : int * int;
    health_point : int;
    attack_point : int;
    jetpack_carburant_pourcentage : int;
    has_grappin : bool;
    sprite_img_name : string
}


val deplacer : joueur -> int * int -> joueur

val drawme : joueur -> Raylib.Texture.t

val create_personnage : string -> string -> joueur

(*val use_jetpack : joueur -> deplacement -> joueur

val use_grappin : joueur -> deplacement -> joueur

val attack : joueur -> hitbox
*)