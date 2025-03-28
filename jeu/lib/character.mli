
type position = float * float

type character = {
  pos : position;
  vector_velocity : float * float;
  sprite : string;
  height : float;
  width : float;
  facing_right : bool;
  airborn : bool;
}

val create_character : float * float -> string -> float -> float -> character

val vel : character -> character

val deplacer : character -> character

val airb : character -> bool -> character