type deplacement =
  | HAUT
  | BAS
  | GAUCHE
  | DROITE

type position = float *float

type hitboxrectangle = position * position * position * position

  type joueur = {
    nom : string;
    pos : position;
    vector_velocity : float * float;
    is_moving_right : bool;
    is_moving_left : bool;
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

let create_personnage nom img h w = 
  {nom = nom;
  pos = (50. , (650.0 -. 92.0));
  is_moving_right = false;
  is_moving_left = false;
  vector_velocity = (0.,0.);
  health_point = 100;
  attack_point = 10;
  jetpack_carburant_pourcentage = 0;
  has_grappin = false;
  sprite= img;
  sprite_height = h;
  sprite_width = w;
  facing_right = true;
  is_jumping =false}
;;

(* let drawme player = Raylib.load_texture player.sprite_img_name *)

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

let deplacer player =
  let (x, y) = player.pos in
  let new_pos = (x +. fst player.vector_velocity , y +. snd player.vector_velocity)
  in
  {nom = player.nom;
  pos = new_pos;
  vector_velocity = player.vector_velocity;
  is_moving_right = player.is_moving_right;
  is_moving_left = player.is_moving_left;
  health_point = player.health_point;
  attack_point = player.attack_point;
  jetpack_carburant_pourcentage = player.jetpack_carburant_pourcentage;
  has_grappin = player.has_grappin;
  sprite = player.sprite;
  sprite_height = player.sprite_height;
  sprite_width = player.sprite_width;
  facing_right = player.facing_right;
  is_jumping = player.is_jumping
  }

let vel player v =
  let (x, y) = player.vector_velocity in
  let new_vel = (x +. fst v , y +. snd v)
  in
  {nom = player.nom;
  pos = player.pos;
  vector_velocity = new_vel;
  is_moving_right = player.is_moving_right;
  is_moving_left = player.is_moving_left;
  health_point = player.health_point;
  attack_point = player.attack_point;
  jetpack_carburant_pourcentage = player.jetpack_carburant_pourcentage;
  has_grappin = player.has_grappin;
  sprite = player.sprite;
  sprite_height = player.sprite_height;
  sprite_width = player.sprite_width;
  facing_right = player.facing_right;
  is_jumping = player.is_jumping
  }

  let moving_right player move_right =
    {nom = player.nom;
    pos = player.pos;
    vector_velocity = player.vector_velocity;
    is_moving_right = move_right;
    is_moving_left = player.is_moving_left;
    health_point = player.health_point;
    attack_point = player.attack_point;
    jetpack_carburant_pourcentage = player.jetpack_carburant_pourcentage;
    has_grappin = player.has_grappin;
    sprite = player.sprite;
    sprite_height = player.sprite_height;
    sprite_width = player.sprite_width;
    facing_right = true;
    is_jumping = player.is_jumping
    }

    let moving_left player move_left=
      {nom = player.nom;
      pos = player.pos;
      vector_velocity = player.vector_velocity;
      is_moving_right = player.is_moving_right;
      is_moving_left = move_left;
      health_point = player.health_point;
      attack_point = player.attack_point;
      jetpack_carburant_pourcentage = player.jetpack_carburant_pourcentage;
      has_grappin = player.has_grappin;
      sprite = player.sprite;
      sprite_height = player.sprite_height;
      sprite_width = player.sprite_width;
      facing_right = false;
      is_jumping = player.is_jumping
      }

  (*
  let modify_player player p v h a j g =
  {nom = player.nom;
  pos = p;
  vector_velocity = v;
  health_point = h;
  attack_point = a;
  jetpack_carburant_pourcentage = j;
  has_grappin = g;
  sprite = player.sprite
  }
  *)