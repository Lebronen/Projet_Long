type plateforme = {
  platform_x : int;
  platform_y : int;
  platform_width : int;
  platform_height : int;
}

type entities = {
  player : joueur;
  ennemis : ennemi list;
  plateforme_list : plateforme list;
}

type position = float * float

type joueur = {
  nom : string;
  pos : position;
  vector_velocity : float * float;
  health_point : int;
  attack_point : int;
  jetpack_carburant_pourcentage : int;
  grap : grappin;
  sprite : string;
  height : float;
  width : float;
  facing_right : bool;
  falling : bool
}
type grappin = {
  pos : position;
  using : bool
  }
