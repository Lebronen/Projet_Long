type deplacement =
  | HAUT
  | BAS
  | GAUCHE
  | DROITE

type joueur = {
  nom : string;
  position : int * int;
  jetpack_carburant_pourcentage : int;
  sprite_img_name : string

}