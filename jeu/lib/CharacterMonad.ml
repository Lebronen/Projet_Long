open Character

type 'a t = character -> 'a * character
let return x = fun j -> (x, j)

let bind m f = fun j ->
  let (x, j') = m j in
  f x j'

let ( let* ) = bind

let get = fun j -> (j, j)

  (* let create_character role_type s x y h w =
    let role = match role_type with
      | "joueur" ->
          Joueur {
            grap = { pos = (0., 0.); using = false };
            jetpack_carburant_pourcentage = 100;
            health_point = 100;
          }
      | "ennemi" ->
          Ennemi {
            health_point = 100;
          }
      | _ -> failwith "Type de rôle inconnu"
    in
    {
      role;
      pos = (x, y);
      vector_velocity = (0., 0.);
      sprite = s;
      height = h;
      width = w;
      facing_right = true;
      airborn = false;
    } *)

    let create_character int s x y h w =
      let role = match int with
        | 0 ->
            Joueur {
              grap = { pos = (0., 0.); using = false };
              jetpack_carburant_pourcentage = 100;
              health_point = 100;
            }
        | 1 ->
            Ennemi {
              health_point = 100;
            }
        | _ -> failwith "Type de rôle inconnu"
      in
      {
        role;
        pos = (x, y);
        vector_velocity = (0., 0.);
        sprite = s;
        height = h;
        width = w;
        facing_right = true;
        airborn = false;
      }

let set_vector_velocity (vx, vy) : unit t = fun character ->
  ((), { character with vector_velocity = (vx, vy) })

  let add_vector_velocity (vx, vy) : unit t = fun character ->
    ((), { character with vector_velocity = (vx +. fst character.vector_velocity, vy +. snd character.vector_velocity) })

  let airb b : unit t = fun character ->
    ((), { character with airborn = b })

  let deplacement (vx, vy) : unit t = fun character ->
    let fr = if fst character.vector_velocity>0. then true else if fst character.vector_velocity<0. then false else character.facing_right in
    ((), { character with pos = (vx, vy); facing_right = fr; })
    
    let set_grappin (using : bool) (pos : float * float) : unit t = fun c ->
      match c.role with
      | Joueur j ->
          let new_grap = { using; pos } in
          let new_joueur = { j with grap = new_grap } in
          ((), { c with role = Joueur new_joueur })
      | _ -> ((), c)

(** Définit les points de vie du joueur *)
let set_health (hp : int) : unit t = fun c ->
  match c.role with
  | Joueur j ->
      let new_joueur = { j with health_point = hp } in
      ((), { c with role = Joueur new_joueur })
  | Ennemi _ ->
      let new_ennemi = { health_point = hp } in
      ((), { c with role = Ennemi new_ennemi })


(** Change les points de vie en les ajoutant ou retirant *)
let add_health (delta : int) : unit t = fun c ->
  match c.role with
  | Joueur j ->
      let new_hp = j.health_point + delta in
      let new_joueur = { j with health_point = new_hp } in
      ((), { c with role = Joueur new_joueur })
  | Ennemi e ->
      let new_hp = e.health_point + delta in
      let new_ennemi = { health_point = new_hp } in
      ((), { c with role = Ennemi new_ennemi })

(** Définit le carburant du jetpack *)
let set_carburant (p : int) : unit t = fun c ->
  match c.role with
  | Joueur j ->
      let new_joueur = { j with jetpack_carburant_pourcentage = p } in
      ((), { c with role = Joueur new_joueur })
  | _ -> ((), c)

(** Modifie le carburant du jetpack en ajoutant ou retirant un pourcentage *)
let add_carburant (delta : int) : unit t = fun c ->
  match c.role with
  | Joueur j ->
      let new_p = j.jetpack_carburant_pourcentage + delta in
      let new_joueur = { j with jetpack_carburant_pourcentage = new_p } in
      ((), { c with role = Joueur new_joueur })
  | _ -> ((), c)

  let get_health_point (c : character) : int =
    match c.role with
      | Joueur j -> j.health_point
      | Ennemi e -> e.health_point
        
  let is_using_grap (c : character) : bool =
    match c.role with
    | Joueur j -> j.grap.using
    | Ennemi _ -> false (* ou failwith si tu veux *)
  
  let get_pos_grap (c : character) : float*float =
    match c.role with
    | Joueur j -> j.grap.pos
    | Ennemi _ -> 0.,0.