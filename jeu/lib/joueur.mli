
type grappin = {
  pos : float * float;
  using : bool
  }

  type t = {
    character : Character.t;
    grap : grappin;
    jetpack_carburant_pourcentage : int;
    health_point : int;
  }


val create_personnage : float * float -> string -> float -> float -> t
