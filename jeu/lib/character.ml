

type position = float * float

  type t = {
    pos : position;
    vector_velocity : float * float;
    sprite : string;
    height : float;
    width : float;
    facing_right : bool;
    airborn : bool;
  }

