(* character.mli *)

type grappin = {
  pos : float * float;
  using : bool;
}

type joueur = {
  grap : grappin;
  jetpack_carburant_pourcentage : int;
  health_point : int;
}

type ennemi = {
  health_point : int;
}

type role =
  | Joueur of joueur
  | Ennemi of ennemi

type character = {
  role : role;
  pos : float * float;
  vector_velocity : float * float;
  sprite : string;
  height : float;
  width : float;
  facing_right : bool;
  airborn : bool;
}
