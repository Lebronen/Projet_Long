open Yojson
open Character
open CharacterMonad


type settings = {
  starttime : float;
  isstartvisible : bool;
  is_game_running : bool;
}

type plateforme = {
  platform_x : int;
  platform_y : int;
  platform_width : int;
  platform_height : int;
}

type entities = {
  player : character;
  ennemis : character;
  plateforme_list : plateforme list;
}


let parse_json file = 
  let json = Basic.from_file file in
  match json with
  |`Assoc l -> if not  (List.length l = 1) then failwith "wrong level format" else (
    let (_, pl) = List.hd l in match pl with
    |`List plist -> List.map (fun p -> 
      match p with
      |`Assoc jp -> List.map (fun champ -> snd champ) jp
      |_ -> failwith "wrong json format"
      ) plist
      |_ -> failwith "wrong json format")
    | _ -> failwith "not a json object"
      
let resolution_X = 1600
let resolution_Y = 900 

  (* let pendule x y x' y' vx vy =
  let r = sqrt((x -. x')**2. +. (y -. y')**2.) in
  (* Étape 1 : Calcul du point (ax, ay) après déplacement *)
  let ax = x +. vx in
  let ay = y +. vy in

  (* Étape 2 : Trouver le point le plus proche du cercle centré en (x', y') *)
  let dx = ax -. x' in
  let dy = ay -. y' in
  let d = sqrt (dx ** 2. +. dy ** 2.) in

  if d = 0. then (r, 0.) (* Éviter division par zéro, vecteur arbitraire *)
  else
    let d_x = dx /. d in
    let d_y = dy /. d in
    let px = x' +. r *. d_x in
    let py = y' +. r *. d_y in

    (* Étape 3 : Calcul du vecteur (vx', vy') de (x, y) vers (px, py) *)
    let vx' = px -. x in
    let vy' = py -. y in
    (vx', vy')        *)

     type direction = Below | Left | Right

    let check_plateforme direction pos vector_velocity height width plateform =
      match direction with
      | Below ->
          (snd vector_velocity +. snd pos -. height) < float_of_int plateform.platform_y
          && (snd pos -. height) >= float_of_int plateform.platform_y
          && (fst pos +. width -. 15.) > float_of_int plateform.platform_x
          && (fst pos +. 15.) < (float_of_int plateform.platform_x +. float_of_int plateform.platform_width)
      | Right ->
          (fst vector_velocity +. fst pos +. width) > float_of_int plateform.platform_x
          && (fst pos +. width) <= float_of_int plateform.platform_x
          && (snd pos -. height) < float_of_int plateform.platform_y
          && (snd pos) > (float_of_int plateform.platform_y -. float_of_int plateform.platform_height)
      | Left ->
          (fst vector_velocity +. fst pos) < (float_of_int plateform.platform_x +. float_of_int plateform.platform_width)
          && (fst pos) >= (float_of_int plateform.platform_x +. float_of_int plateform.platform_width)
          && (snd pos -. height) < float_of_int plateform.platform_y
          && (snd pos) > (float_of_int plateform.platform_y -. float_of_int plateform.platform_height)

    let is_on_plateforme direction (c: character) p_list =
      List.exists (check_plateforme direction c.pos c.vector_velocity c.height c.width) p_list

    (* let is_on_plateforme_ennemi direction (enemy: ennemi) p_list =
      List.exists (check_plateforme direction enemy.pos enemy.vector_velocity enemy.height enemy.width) p_list *)

    let wich_plateforme direction (c: character) p_list =
      List.filter (check_plateforme direction c.pos c.vector_velocity c.height c.width) p_list

    (* let wich_plateforme_ennemi direction (enemy: ennemi) p_list =
      List.filter (check_plateforme direction enemy.pos enemy.vector_velocity enemy.height enemy.width) p_list *)
   

let setup () =
  Raylib.init_window resolution_X resolution_Y "L'ATTAQUE DES TITOUAN";
  Raylib.set_target_fps 60;

  let menu_texture = Raylib.load_texture "../resources/PNG/background 2/Preview 2.png" in
  let player = create_character "joueur" "../resources/spritesheetcourse.png" 200. 350. 100. 70. in
  (* let enemy = create_personnage "ennemi" "../resources/blue.png" 100. 100. 1200. 650. in *)
  let enemy = create_character "ennemy" "../resources/blue.png" 1174. 650. 100. 100. in


  let sprite_texture = Raylib.load_texture player.sprite in
  let enemy_texture = Raylib.load_texture enemy.sprite in

  let parsed_list = parse_json "../resources/level.json" in

  let p_list = List.init (List.length parsed_list) (fun i -> 
    let l = List.nth parsed_list i in
    match l with
    |x::y::w::h::[] -> (match (x,y,w,h) with
    |(`Int xi, `Int yi, `Int wi, `Int hi) -> {platform_x = xi; platform_y = yi; platform_width = wi; platform_height = hi;}
    | _ -> failwith "wrong json format")
    | _ -> failwith "wrong json format"
    ) in
  let entities = { player; ennemis = enemy; plateforme_list = p_list } in
  (menu_texture, sprite_texture, enemy_texture, entities)

let start_time = ref (Raylib.get_time ())
let is_start_visible = ref true
let is_game_running = ref false

let rec loop menu_texture sprite_texture enemy_texture entities frame =
  if Raylib.window_should_close () then (
    Raylib.unload_texture menu_texture;
    Raylib.unload_texture sprite_texture;
    Raylib.close_window ()
  )
  else
    let open Raylib in
    if not !is_game_running then begin
      let current_time = get_time () in
      if current_time -. !start_time >= 0.7 then begin
        is_start_visible := not !is_start_visible;
        start_time := current_time
      end;
      if is_key_pressed Key.Enter then is_game_running := true;
    end;
     let entities = if !is_game_running then 
     ( let joueur = entities.player in
        let actionjoueur =
          (* déplacement latéral *)
          let* () =
            match (is_key_down Key.Right, is_key_down Key.Left) with
            | true, false ->
                let* joueur = get in
                if fst joueur.vector_velocity < 8.0 then
                  add_vector_velocity (8.0, 0.0)
                else
                  return ()
            | false, true ->
                let* joueur = get in
                if fst joueur.vector_velocity > -8.0 then
                  add_vector_velocity (-8.0, 0.0)
                else
                  return ()
            | _, _ ->
                let* joueur = get in
                if not joueur.airborn then
                  add_vector_velocity (-. (fst joueur.vector_velocity), 0.0)
                else
                  return ()
          in
        
          (* Gravité *)
          let* () = add_vector_velocity (0.0,-1.)
        in

          (* Collision avec le sol*)
          let* joueur = get in
          let y = snd joueur.vector_velocity +. snd joueur.pos -. joueur.height in
          let* () =
            if y < 0. then (
              let vy = snd joueur.vector_velocity +. snd joueur.pos -. joueur.height in
              let* () = add_vector_velocity (0., -.vy) in
              airb false
            ) else return ()
          in

          (* Collision avec plateformes en dessous *)
          let* joueur = get in
          let* () =
            if is_on_plateforme Below joueur entities.plateforme_list && not (is_using_grap joueur) then
              let p = List.nth (wich_plateforme Below joueur entities.plateforme_list) 0 in
              let vy = snd joueur.vector_velocity -. (float_of_int p.platform_y -. (snd joueur.pos -. joueur.height)) in
              let* () = add_vector_velocity (0., -.vy) in
              airb false
            else return ()
          in

          (* Grappin *)
        (*  
        let* joueur = get in
          let* () =
            if is_key_down Key.Space then
              let (vx', vy') = pendule 
                (fst joueur.character.pos +. (joueur.character.width /. 2.)) 
                (snd joueur.character.pos) 
                (fst joueur.grap.pos) 
                (snd joueur.grap.pos) 
                (fst joueur.character.vector_velocity) 
                (snd joueur.character.vector_velocity)
              in
              let* () =
                if joueur.grap.using then
                  let* () = airb true in
                  return () (* Continue si le grappin est déjà utilisé *)
                else
                  let* () =
                    if joueur.character.facing_right then
                      let* () = airb true in
                      set_grappin true (fst joueur.character.pos +. 250. +. (joueur.character.width /. 2.), snd joueur.character.pos +. 150.)
                    else
                      let* () = airb true in
                      set_grappin true (fst joueur.character.pos -. 250. +. (joueur.character.width /. 2.), snd joueur.character.pos +. 150.)
                  in
                  return () (* Grappin activé et position mis à jour *)
              in
              let* () = add_vector_velocity (-.fst joueur.character.vector_velocity +. vx', -.snd joueur.character.vector_velocity +. vy') in
              return () (* Vitesse mise à jour *)
            else
              let* () = set_grappin false (0.0, 0.0) in
              return () (* Si la touche Space n'est pas pressée, désactiver le grappin et réinitialiser la position *)
          in
          *)
        
          (* Collision droite *)
          let* joueur = get in
          let* () =
            if is_on_plateforme Right joueur entities.plateforme_list then
              let p = List.nth (wich_plateforme Right joueur entities.plateforme_list) 0 in
              let vx = fst joueur.vector_velocity -. (float_of_int p.platform_x -. (fst joueur.pos +. joueur.width)) in
              add_vector_velocity (-.vx, 0.)
            else return ()
          in

          (* Collision gauche *)
          let* joueur = get in
          let* () =
            if is_on_plateforme Left joueur entities.plateforme_list then
              let p = List.nth (wich_plateforme Left joueur entities.plateforme_list) 0 in
              let vx = fst joueur.vector_velocity -. ((float_of_int p.platform_x +. float_of_int p.platform_width) -. fst joueur.pos) in
              add_vector_velocity (-.vx, 0.)
            else return ()
          in
          
          (* Saut *)
          let* joueur = get in
            let* () =
            if is_key_down Key.Up && not joueur.airborn then (
              let* () = add_vector_velocity (0., 20.) in
              airb true 
            ) else
              return ()
          in

          (* Déplacement *)
          let* joueur = get in
            let* () = deplacement
              (fst joueur.pos +. fst joueur.vector_velocity,
              snd joueur.pos +. snd joueur.vector_velocity)
            in
            return ()
          in

        let (_, joueur) = actionjoueur joueur in
        {player = joueur; ennemis = entities.ennemis; plateforme_list = entities.plateforme_list}
      )  
      else entities in 

    
    let draw_game entities = 
      begin_drawing (); 
      clear_background Color.white;
      
      if !is_game_running then begin
        let origin = Vector2.create 0. 0. in
        let menu_source = Rectangle.create 0. 0. (4608.) (2592.) in
        let menu_dest_rect = Rectangle.create 0. 0. (float_of_int resolution_X) (float_of_int resolution_Y) in
        draw_texture_pro menu_texture menu_source menu_dest_rect origin 0. Color.white;

        (* affichage du joueur *)
        let player = entities.  player in
        let f = 
          if is_using_grap player then 0.
          else
          match player.vector_velocity with
          |(0.,_) -> 0.
          |_ -> match frame with
                |_ when frame<5 -> 0.
                |_ when frame<10 -> 35.
                |_ when frame<15 -> 70.
                |_ when frame<20 -> 105.
                |_ when frame<25 -> 140.
                |_ -> 175.
        in
        let source_rect = Rectangle.create f 0. (if player.facing_right then (35.) else -. (35.)) (50.) in
        let dest_rect = Rectangle.create (fst player.pos) (float_of_int(resolution_Y) -. (snd player.pos)) (player.width) (player.height) in 
        let origin = Vector2.create 0. 0. in
        draw_texture_pro sprite_texture source_rect dest_rect origin 0. Color.white;  
        if (is_using_grap player) then draw_line (int_of_float(fst player.pos +. (player.width /. 2.))) (resolution_Y - int_of_float(snd player.pos)) (int_of_float(fst (get_pos_grap player))) (resolution_Y - int_of_float(snd (get_pos_grap player))) Color.black; 
        draw_rectangle 50 50 (3*(get_health_point player)) 20 Color.green;

        (* affichage ennemis *)
        let enemy = entities.ennemis in
        let enemy_dest_rect = Rectangle.create (fst enemy.pos) (float_of_int(resolution_Y) -. (snd enemy.pos)) (enemy.width) (enemy.height) in
        draw_texture_pro enemy_texture source_rect enemy_dest_rect origin 0. Color.white;
        
        (* affichage plateforme *)
        List.iter (fun p -> draw_rectangle p.platform_x (resolution_Y - p.platform_y) p.platform_width p.platform_height Color.brown) entities.plateforme_list;
      end else begin

        if !is_start_visible then draw_text "Appuyez sur entrée pour commencer" 350 400 50 Color.red;
      end;
      end_drawing (); 
    in
    draw_game entities;
    loop menu_texture sprite_texture enemy_texture entities (if (frame==30) then 0 else (frame+1))

let gameloop () =
  let menu_texture, sprite_texture, enemy_texture, entities = setup () in
  loop menu_texture sprite_texture enemy_texture entities 0