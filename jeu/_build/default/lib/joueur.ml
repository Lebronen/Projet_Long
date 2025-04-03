
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
    vel : float -> float -> unit CharacterM.CharacterMonad.t;
    deplacer : unit CharacterM.CharacterMonad.t;
    airb : bool -> unit CharacterM.CharacterMonad.t
  }
  let vel_f (x : float) (y : float) = 
    CharacterM.vel x y

  let deplacer_f = CharacterM.deplacer

  let airb_f (is_airborn : bool) = CharacterM.airb is_airborn
 
let create_personnage (x, y) sprite height width = 
  {
    character = CharacterM.create_character (x, y) sprite height width;
    grap = {
      pos = (0.,0.);
      using = false
    };
    jetpack_carburant_pourcentage = 0;
    health_point = 100;
    vel = vel_f;
    deplacer = deplacer_f;
    airb = airb_f
  }
