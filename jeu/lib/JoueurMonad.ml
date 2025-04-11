  type 'a t = Joueur.t -> 'a * Joueur.t

  let return x = fun j -> (x, j)

  let bind m f = fun j ->
    let (x, j') = m j in
    f x j'

  (* Syntaxe let* pour plus de lisibilité *)
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
  (* Tu peux ajouter d'autres opérations ici, comme changer la position, etc. *)
