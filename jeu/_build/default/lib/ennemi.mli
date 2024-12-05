type deplacement =
  | HAUT
  | BAS
  | GAUCHE
  | DROITE

type position = int *int

type hitbox = position * position * position * position

type ennemi = {
    nom : string;
    health_point : int;
    attack_point : int;
    pos : position;
    sprite_img_name : string
}

val deplacer : ennemi -> deplacement -> ennemi

val drawme : ennemi -> unit

val create_personnage : string -> string -> ennemi

val attack : ennemi -> hitbox