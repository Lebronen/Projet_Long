  type 'a t = Joueur.t -> 'a * Joueur.t

  let return x = fun j -> (x, j)

  let bind m f = fun j ->
    let (x, j') = m j in
    f x j'

  let ( let* ) = bind

  let get = fun j -> (j, j)

  let set_vector_velocity (vx, vy) : unit t = fun joueur ->
    let character = { joueur.character with vector_velocity = (vx, vy) } in
    ((), { joueur with character })

    let add_vector_velocity (vx, vy) : unit t = fun joueur ->
      let character = { joueur.character with vector_velocity = (vx +. fst joueur.character.vector_velocity, vy +. snd joueur.character.vector_velocity) } in
      ((), { joueur with character })

    let airb b : unit t = fun joueur ->
      let character = { joueur.character with airborn = b} in
      ((), { joueur with character })

    let deplacement (vx, vy) : unit t = fun joueur ->
      let fr = if fst joueur.character.vector_velocity>0. then true else if fst joueur.character.vector_velocity<0. then false else joueur.character.facing_right in
      let character = { joueur.character with pos = (vx, vy); facing_right = fr; } in
      ((), { joueur with character })
      
    let set_grappin (using : bool) (pos : float * float) : unit t = fun joueur ->
      let new_grap : Joueur.grappin = { using = using; pos = pos } in
      ((), { joueur with grap = new_grap })
      
(** Définit les points de vie du joueur *)
let set_health (hp : int) : unit t = fun joueur ->
  ((), { joueur with health_point = hp })

(** Change les points de vie en les ajoutant ou retirant *)
let add_health (delta : int) : unit t = fun joueur ->
  let hp = joueur.health_point + delta in
  ((), { joueur with health_point = hp })

(** Définit le carburant du jetpack *)
let set_carburant (p : int) : unit t = fun joueur ->
  ((), { joueur with jetpack_carburant_pourcentage = p })

(** Modifie le carburant du jetpack en ajoutant ou retirant un pourcentage *)
let add_carburant (delta : int) : unit t = fun joueur ->
  let new_p = joueur.jetpack_carburant_pourcentage + delta in
  ((), { joueur with jetpack_carburant_pourcentage = new_p })
