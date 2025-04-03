
type grappin = {
  pos : float * float;
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


val create_personnage : float * float -> string -> float -> float -> t

(*

val use_grappin : joueur -> deplacement -> joueur

*)