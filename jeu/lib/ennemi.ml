type ennemi = {
    character : Character.t;
    health_point : int;
}

let create_ennemi sprite height width x y =
  {
  character = { pos = (x, y);
  vector_velocity = (0., 0.);
  sprite;
  height;
  width;
  facing_right = true;
  airborn = false };
  health_point = 100;
  }