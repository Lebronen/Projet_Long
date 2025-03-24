type position = float * float

type hitboxrectangle = position * position * position * position

type grappin = {
  pos : position;
  using : bool
  }

type joueur = {
    nom : string;
    pos : position;
    vector_velocity : float *float;
    health_point : int;
    jetpack_carburant_pourcentage : int;
    grap : grappin;
    sprite : string;
    height : float;
    width : float;
    facing_right : bool;
    airborn : bool
}


val deplacer : joueur -> joueur

val vel : joueur -> float * float -> joueur

val airb : joueur -> bool -> joueur

val grapin : joueur -> bool -> position -> joueur

val carbu : joueur -> joueur

val create_personnage : string -> string -> float -> float -> float -> float -> joueur

(*

val use_grappin : joueur -> deplacement -> joueur

*)