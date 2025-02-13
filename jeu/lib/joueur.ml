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

let create_personnage nom img = 
  {nom = nom;
  pos = (0, 650);
  vector_velocity = (0,0);
  health_point = 100;
  attack_point = 10;
  jetpack_carburant_pourcentage = 0;
  has_grappin = false;
  sprite_img_name = img}
;;

let drawme player = Raylib.load_texture player.sprite_img_name

(* let deplacer player dp = 
  let (x, y) = player.pos in
  let new_pos = match dp with
    |HAUT -> (x, y - 5)
    |BAS -> (x, y + 5)
    |GAUCHE -> (x - 5, y)
    |DROITE -> (x + 5, y)
  in
  {nom = player.nom;
  pos = new_pos;
  health_point = player.health_point;
  attack_point = player.attack_point;
  jetpack_carburant_pourcentage = player.jetpack_carburant_pourcentage;
  has_grappin = player.has_grappin;
  sprite_img_name = player.sprite_img_name
  }
;; *)

let deplacer player v =
  let (x, y) = player.pos in
  let new_pos = (x + fst v , y + snd v)
  in
  {nom = player.nom;
  pos = new_pos;
  vector_velocity = player.vector_velocity;
  health_point = player.health_point;
  attack_point = player.attack_point;
  jetpack_carburant_pourcentage = player.jetpack_carburant_pourcentage;
  has_grappin = player.has_grappin;
  sprite_img_name = player.sprite_img_name
  }