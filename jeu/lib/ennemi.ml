type position = float * float

type hitbox = position * position * position * position

type ennemi = {
    nom : string;
    pos : position;
    vector_velocity : float * float;
    health_point : int;
    sprite : string;
    height : float;
    width : float;
    facing_right : bool;
}

let create_ennemi nom img h w px py =
  {nom = nom;
  pos = (px,py);
  vector_velocity = (0.,0.);
  health_point = 100;
  sprite = img;
  height = h;
  width = w;
  facing_right = false;
  }