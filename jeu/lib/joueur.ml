
  type position = float * float


  type grappin = {
  pos : position;
  using : bool
  }

  type t = {
    character : Character.t;
    grap : grappin;
    jetpack_carburant_pourcentage : int;
    health_point : int;
  }
 
let create_personnage (x, y) sprite height width = 
  {
    character = { pos = (x, y);
    vector_velocity = (0., 0.);
    sprite;
    height;
    width;
    facing_right = true;
    airborn = false };
    grap = {
      pos = (0.,0.);
      using = false
    };
    jetpack_carburant_pourcentage = 0;
    health_point = 100;
  }
