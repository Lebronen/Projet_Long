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
