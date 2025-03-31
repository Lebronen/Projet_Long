
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
 
