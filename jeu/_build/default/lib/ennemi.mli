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

val create_ennemi : string -> string -> float -> float -> float -> float -> ennemi

val vele : ennemi -> float * float -> ennemi

val deplace : ennemi -> ennemi

(*

val create_personnage : string -> string -> ennemi

*)