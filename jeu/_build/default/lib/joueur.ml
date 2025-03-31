
  type position = float * float


  type grappin = {
  pos : position;
  using : bool
  }

  type t = {
    character : Character.t;
    grap : grappin;
    jetpack_carburant_pourcentage : int;
    health_point : int
  }
 
let create_personnage (x, y) sprite height width = 
  {
    character = CharacterM.create_character (x, y) sprite height width;
    grap = {
      pos = (0.,0.);
      using = false
    };
    jetpack_carburant_pourcentage = 0;
    health_point = 100
  }

  let vel (x : float) (y : float) = 
    CharacterM.vel x y

  let deplacer = CharacterM.deplacer

  let airb (is_airborn : bool) = CharacterM.airb is_airborn