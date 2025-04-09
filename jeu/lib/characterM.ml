module type CharacterMonad = sig
  type  'a t 
end

module CharacterMonad = struct
  type 'a t = Character.t -> 'a * Character.t

   (* let return (x : 'a) : 'a t = fun c -> (x, c) *)

  let bind (m : 'a t) (f : 'a -> 'b t) : 'b t =
    fun c ->
      let (x, c') = m c in
      f x c'

  (* Opérateur d'enchaînement *)
  let ( let* ) = bind

  (* Modification d'un character *)
  let modify (f : Character.t -> Character.t) : unit t =
    fun c -> ((), f c)

  (* Récupération de l'état actuel *)
  let get : Character.t t =
    fun c -> (c, c)

  (* Récupération d'une valeur spécifique *)
  (* let get_field (f : Character.t -> 'a) : 'a t =
    fun c -> (f c, c) *)
end

open CharacterMonad

(* Création d'un personnage *)
let create_character (x, y) sprite height width : Character.t =
  { pos = (x, y);
    vector_velocity = (0., 0.);
    sprite;
    height;
    width;
    facing_right = true;
    airborn = false }

(* mise à jour de la vélocité *)
let vel (x : float) (y : float) : unit t =
  modify (fun c -> { c with vector_velocity = (fst c.vector_velocity +. x, snd c.vector_velocity +. y) })

(* Déplace le personnage *)
let deplacer : unit t =
  let* c = get in
  let new_pos = (fst c.pos +. fst c.vector_velocity, snd c.pos +. snd c.vector_velocity) in
  let new_facing_right = if fst c.vector_velocity > 0. then true 
    else if fst c.vector_velocity < 0. then false 
      else c.facing_right in
  modify (fun c -> { c with pos = new_pos; facing_right = new_facing_right })

(* Met à jour l'état "airborn" *)
let airb (is_airborn : bool) : unit t =
  modify (fun c -> { c with airborn = is_airborn })
