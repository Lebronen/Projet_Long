
  type position = float * float

  type hitboxrectangle = position * position * position * position
  (* haut gauche, haut droite, bas droite, bas gauche *)

  type grappin = {
  pos : position;
  using : bool
  }

  type joueur = {
    nom : string;
    pos : position;
    vector_velocity : float * float;
    health_point : int;
    attack_point : int;
    jetpack_carburant_pourcentage : int;
    grap : grappin;
    sprite : string;
    height : float;
    width : float;
    facing_right : bool;
    is_jumping : bool
}

let create_personnage nom img h w px py = 
  {nom = nom;
  pos = (px, (py -. h));
  vector_velocity = (0.,0.);
  health_point = 100;
  attack_point = 10;
  jetpack_carburant_pourcentage = 500;
  grap = {
    pos = (0. ,0.);
    using = false;
  };
  sprite = img;
  height = h;
  width = w;
  facing_right = true;
  is_jumping = false}
;;

let deplacer player =
  {player with 
  pos = (fst player.pos +. fst player.vector_velocity, snd player.pos +. snd player.vector_velocity);
  facing_right = if (fst player.vector_velocity > 0.) then true else if (fst player.vector_velocity < 0.) then false else (player.facing_right);
  }

  let vel player v =
    { player with 
    vector_velocity = (fst v +. fst player.vector_velocity, snd v +. snd player.vector_velocity)
    }

  let jump player b =
    { player with 
    is_jumping = b;
    }
  
  let grapin player b p =
  {
    player with
    grap = {
      pos = p;
      using = b;
    };
  }

  let carbu player =
    {
      player with
      jetpack_carburant_pourcentage = player.jetpack_carburant_pourcentage -1;
    }