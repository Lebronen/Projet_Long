type ennemi = {
    character : Character.t;
    health_point : int;
}

val create_ennemi : string ->  float -> float -> float -> float -> ennemi