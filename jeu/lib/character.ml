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

(* Définition de la monade CharacterM *)
module CharacterM = struct
  type 'a t = character -> 'a * character

  let return (x : 'a) : 'a t = fun c -> (x, c)

  let bind (m : 'a t) (f : 'a -> 'b t) : 'b t =
    fun c ->
      let (x, c') = m c in
      f x c'

  (* Opérateur d'enchaînement *)
  let ( let* ) = bind

  (* Modification d'un character *)
  let modify (f : character -> character) : unit t =
    fun c -> ((), f c)

  (* Récupération de l'état actuel *)
  let get : character t =
    fun c -> (c, c)

  (* Récupération d'une valeur spécifique *)
  let get_field (f : character -> 'a) : 'a t =
    fun c -> (f c, c)
end

open CharacterM

(* Création d'un personnage *)
let create_character (x, y) sprite height width : character =
  { pos = (x, y);
    vector_velocity = (0., 0.);
    sprite;
    height;
    width;
    facing_right = true;
    airborn = false }

(* Met à jour la vélocité *)
let vel : unit t =
  modify (fun c ->
    let (vx, vy) = c.vector_velocity in
    { c with pos = (fst c.pos +. vx, snd c.pos +. vy) })

(* Déplace le personnage *)
let deplacer : unit t =
  let* c = get in
  let new_pos = (fst c.pos +. fst c.vector_velocity, snd c.pos +. snd c.vector_velocity) in
  modify (fun c -> { c with pos = new_pos })

(* Met à jour l'état "airborn" *)
let airb (is_airborn : bool) : unit t =
  modify (fun c -> { c with airborn = is_airborn })
