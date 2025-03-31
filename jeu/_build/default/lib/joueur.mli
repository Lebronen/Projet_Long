
type grappin = {
  pos : float * float;
  using : bool
  }

  type t = {
    character : Character.t;
    grap : grappin;
    jetpack_carburant_pourcentage : int;
    health_point : int
  }


(*

val use_grappin : joueur -> deplacement -> joueur

*)