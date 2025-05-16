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
  ennemis : (character * float * float) list;
  plateforme_list : plateforme list;
}

(* parsing du fichier json des niveaux *)
let parse_json file = 
  let json = Basic.from_file file in
  match json with
  |`Assoc l -> if not  (List.length l = 2) then failwith "wrong level format" else (
    let p_list =
    (let (_, pl) = List.hd l in match pl with
    |`List plist -> List.map (fun p -> 
      match p with
      |`Assoc jp -> List.map (fun champ -> snd champ) jp
      |_ -> failwith "wrong json format"
      ) plist
    |_ -> failwith "wrong json format")
    in let e_list = 
     (let (_, el) = List.nth l 1 in match el with
   |`List elist -> List.map (fun e ->
      match e with
      | `Assoc je -> List.map (fun champ -> snd champ) je
      | _ -> failwith "wrong json format"
      ) elist
      | _ -> failwith "wrong json format")
   in (p_list, e_list)
   )
    | _ -> failwith "not a json object"
  
      
let resolution_X = 1600
let resolution_Y = 900 


  let pendule x y x' y' vx vy =
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
    (vx', vy')       

    (* verification des collisions avec les plateformes *)
    type direction = Below | Left | Right
    let check_plateforme direction pos vector_velocity height width plateform =
      match direction with
      | Below ->
          (snd vector_velocity +. snd pos -. height) < float_of_int plateform.platform_y
          && (snd pos -. height) >= float_of_int plateform.platform_y
          && (fst pos +. width) > float_of_int plateform.platform_x
          && (fst pos) < (float_of_int plateform.platform_x +. float_of_int plateform.platform_width)
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

    let wich_plateforme direction (c: character) p_list =
      List.filter (check_plateforme direction c.pos c.vector_velocity c.height c.width) p_list

let setup () =
  (* Raylib.init_window resolution_X resolution_Y "L'ATTAQUE DES TITOUAN";
  Raylib.set_target_fps 60; *)

  (* loading des textures *)
  let menu_texture = Raylib.load_texture "../resources/bg_menu.png" in
  let bg_texture = Raylib.load_texture "../resources/PNG/background 1/Preview 1.png" in
  let player = create_character 0 "../resources/sp.png" 0. 100. 100. 70. in
  let parsed_list = parse_json "../resources/level/level1.json" in
  let vie_texture = Raylib.load_texture "../resources/vie.png" in
  let carburant_texture = Raylib.load_texture "../resources/lightning.png" in
  let porte_texture = Raylib.load_texture "../resources/porte.png" in
  let sprite_texture = Raylib.load_texture player.sprite in

  let pennemis = List.init (List.length (snd parsed_list)) (fun i ->
    let l = List.nth (snd parsed_list) i in
    match l with
    |s::xmin::xmax::y::[] -> (match (s,xmin,xmax,y) with
      |(`String si, `Float xmini, `Float xmaxi, `Float yi) -> (create_character 1 si xmini yi 150. 150., xmini, xmaxi)
      | _ -> failwith "wrong json format"
      )
    | _ -> failwith "wrong json format"
    )
  in
  
  let enemy_textures = List.init (List.length pennemis) (fun i -> let (e, _, _) = List.nth pennemis i in
    Raylib.load_texture e.sprite) in

  let p_list = List.init (List.length (fst parsed_list)) (fun i -> 
    let l = List.nth (fst parsed_list) i in
    match l with
    |x::y::w::h::[] -> (match (x,y,w,h) with
    |(`Int xi, `Int yi, `Int wi, `Int hi) -> {platform_x = xi; platform_y = yi; platform_width = wi; platform_height = hi;}
    | _ -> failwith "wrong json format")
    | _ -> failwith "wrong json format"
    ) in
  let entities = { player; ennemis = pennemis; plateforme_list = p_list } in
  (menu_texture, bg_texture, sprite_texture, enemy_textures, entities, carburant_texture, vie_texture, porte_texture)


let start_time = ref (Raylib.get_time ())
let is_start_visible = ref true
let is_game_running = ref false

let is_game_over = ref false

let reset_game () =
  let (_, _, sprite_texture, enemy_textures, entities, _, _, _) = setup () in
  (sprite_texture, enemy_textures, entities)


let rec loop menu_texture bg_texture sprite_texture enemy_textures entities carburant_texture vie_texture porte_texture level=
  if Raylib.window_should_close () then (
    Raylib.unload_texture menu_texture;
    Raylib.unload_texture bg_texture;
    Raylib.unload_texture sprite_texture;
    Raylib.unload_texture carburant_texture;
    Raylib.unload_texture vie_texture;
    let rec unload_ennemis el = match el with
      |[] -> ()
      |e::es -> ((Raylib.unload_texture e);(unload_ennemis es))
    in unload_ennemis enemy_textures;
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
      if is_key_pressed Key.Enter then 
        is_game_running := true;
    end;
     let entities = if !is_game_running &&  not !is_game_over then
    ( let joueur = entities.player in
        let action =
          (* déplacement latéral *)
          let* () =
            match (is_key_down Key.Right, is_key_down Key.Left) with
            | true, false ->
                let* joueur = get in
                if is_using_grap joueur then 
                  if fst joueur.vector_velocity < 6.0 then
                    add_vector_velocity (1.5, 0.0)
                  else
                    return ()
                else
                if fst joueur.vector_velocity < 8.0 then
                  add_vector_velocity (8.0, 0.0)
                else
                  return ()
            | false, true ->
                let* joueur = get in
                if is_using_grap joueur then 
                  if fst joueur.vector_velocity > -6.0 then
                    add_vector_velocity (-1.5, 0.0)
                  else
                    return ()
                else
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

          (* Passage a la frame suivante du sprite*)
        let* () = add_frame
      in

        (* Grappin *) 
        let* joueur = get in
          let* () =
            if (is_key_down Key.Space && ((get_carburant joueur) > 0))then
              let (vx', vy') = pendule 
                (fst joueur.pos +. (joueur.width /. 2.)) 
                (snd joueur.pos) 
                (fst (get_pos_grap joueur)) 
                (snd (get_pos_grap joueur)) 
                (fst joueur.vector_velocity) 
                (snd joueur.vector_velocity)
              in
              let* () =
                if (is_using_grap joueur) then
                  let* () = airb true in
                  let* () = add_carburant (-1) in
                  return () (* Continue si le grappin est déjà utilisé *)
                else
                  let* () =
                    if joueur.facing_right then
                      let* () = airb true in
                      set_grappin true (fst joueur.pos +. 200. +. (joueur.width /. 2.), snd joueur.pos +. 150.)
                    else
                      let* () = airb true in
                      set_grappin true (fst joueur.pos -. 200. +. (joueur.width /. 2.), snd joueur.pos +. 150.)
                  in
                  return () (* Grappin activé et position mis à jour *)
              in
              let* () = add_vector_velocity (-.fst joueur.vector_velocity +. vx', -.snd joueur.vector_velocity +. vy') in
              return () (* Vitesse mise à jour *)
            else
              let* () = set_grappin false (0.0, 0.0) in
              return () (* Si la touche Space n'est pas pressée, désactiver le grappin et réinitialiser la position *)
          in

          (* Collision avec plateformes en dessous *)
          let* joueur = get in
          let* () =
            if is_on_plateforme Below joueur entities.plateforme_list then
              let p = List.nth (wich_plateforme Below joueur entities.plateforme_list) 0 in
              let vy = snd joueur.vector_velocity -. (float_of_int p.platform_y -. (snd joueur.pos -. joueur.height)) in
              let* () = add_vector_velocity (0., -.vy) in
              airb false
            else airb true
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

          (* Récuperation du jetpack *)
          let* joueur = get in
            let* () = if ((not joueur.airborn) && (get_carburant joueur) <= 100) then add_carburant 1
            else return ()
          in

          (* Attack joueur *)
          
            let* () = if is_key_pressed Key.E && get_frame joueur<30 then
              set_frame 30
            else return ()
           in   

          (* Déplacement *)
          let* joueur = get in
            let* () = deplacement
              (fst joueur.pos +. fst joueur.vector_velocity,
              snd joueur.pos +. snd joueur.vector_velocity)
            in
            return ()
          in
          
          let (_, joueur) = action joueur in


          let collide (x1, y1, w1, h1) (x2, y2, w2, h2) = not (x1 +. w1 < x2 || x2 +. w2 < x1 || y1 -. h1 > y2 || y2 -. h2 > y1) in
          (* perte de point de vie du joueur au contacte d'un ennemi *)
            let update_player_on_enemy_collision (player : character) (enemies : (character * float * float) list) : character =
            let player_box = 
              let x, y = joueur.pos in
              (x, y, joueur.width, joueur.height)
            in
            let collided = 
              List.exists (fun (enemy, _, _) ->
                match enemy.role with
                | Ennemi _ ->
                    let ex, ey = enemy.pos in
                    let enemy_box = (ex, ey, enemy.width, enemy.height) in
                    collide player_box enemy_box
                | _ -> false
              ) enemies
            in
            if collided then
              match player.role with
              | Joueur j ->
                  let new_joueur = { j with health_point = j.health_point - 1 } in
                  { player with role = Joueur new_joueur }
              | _ -> player
            else player
          in 
          let joueur = update_player_on_enemy_collision joueur entities.ennemis
          in

          let () = 
          match joueur.role with 
          |Joueur j -> if j.health_point <= 0 then is_game_over := true
          |_ -> ()
        in

          (* perte de point de vie des ennemis par l'attaque du joueur *)
          let update_ennemis_on_collision (ennemis : (character * float * float) list) (joueur : character) : (character * float * float) list =
            let jx, jy = joueur.pos in
            let jw, jh = joueur.width, joueur.height in
          
            (* On applique l'effet seulement si frame > 30 *)
            if get_frame joueur > 30 then
              let box_joueur =
                if joueur.facing_right then
                  (jx, jy, jw +. 65., jh)
                else
                  (jx -. 65., jy, jw +. 65., jh)
              in
              List.map (fun (ennemi, fx, fy) ->
                let ex, ey = ennemi.pos in
                let ew, eh = ennemi.width, ennemi.height in
                let box_ennemi = (ex, ey, ew, eh) in
                if collide box_joueur box_ennemi then
                  match ennemi.role with
                  | Ennemi e ->
                      let new_e = { e with health_point = e.health_point - 5 } in
                      ({ ennemi with role = Ennemi new_e }, fx, fy)
                  | _ -> (ennemi, fx, fy)
                else
                  (ennemi, fx, fy)
              ) ennemis
            else
              ennemis
              in
          let ennemis = update_ennemis_on_collision entities.ennemis joueur in
        
          let ennemis = (List.map (fun (e, xmin, xmax) -> let (_, enemy) = patrol xmin xmax 2. e  in (enemy,xmin, xmax)) ennemis) in

        {player = joueur; ennemis = ennemis; plateforme_list = entities.plateforme_list}
      )  
      
      else entities in 

        (* suppression des ennemis mort *)
        let filtrer_characters (chars : (Character.character * float * float) list) (bs : 'b list) =
          let rec aux chars bs acc_chars acc_bs = match chars, bs with
            | [], [] -> List.rev acc_chars, List.rev acc_bs
            | (c, f1, f2)::cs, b::bs_tail ->
                let hp = match c.role with
                  | Joueur j -> j.health_point
                  | Ennemi e -> e.health_point
                in
                if hp < 0 then
                  aux cs bs_tail acc_chars acc_bs
                else
                  aux cs bs_tail ((c, f1, f2)::acc_chars) (b::acc_bs)
            | _ -> failwith "Les deux listes doivent avoir la même longueur"
          in
          aux chars bs [] []
        in
        let e, enemy_textures= filtrer_characters entities.ennemis enemy_textures
        in
        let entities = {player = entities.player; ennemis = e; plateforme_list = entities.plateforme_list}
        in


    let draw_game entities = 
      begin_drawing (); 
      clear_background Color.white;

      if !is_game_over then begin
        begin_drawing ();
        clear_background Color.black;
        draw_text "GAME OVER" (resolution_X / 2 - 200) (resolution_Y / 2 - 50) 60 Color.red;
        draw_text "Appuyez sur Echap pour quitter" (resolution_X / 2 - 250) (resolution_Y / 2 + 30) 30 Color.white;
        draw_text "Appuyez sur R pour relancer" (resolution_X / 2 - 200) (resolution_Y / 2 + 80) 30 Color.white;
        draw_text "Appuyez sur M pour revenir au menu" (resolution_X / 2 - 250) (resolution_Y / 2 + 120) 30 Color.white;
        if is_key_pressed Key.R then begin
          let (sprite_texture', enemy_textures', entities') = reset_game () in
          is_game_over := false;
          is_game_running := true;
          loop menu_texture bg_texture sprite_texture' enemy_textures' entities' carburant_texture vie_texture porte_texture level
        end;
        
      
        if is_key_pressed Key.M then begin
          let (sprite_texture', enemy_textures', entities') = reset_game () in
          is_game_over := false;
          is_game_running := false;
          start_time := get_time ();
          loop menu_texture bg_texture sprite_texture' enemy_textures' entities' carburant_texture vie_texture porte_texture level
        end;
        
      
      
        end_drawing ();
      end
    else 
      if !is_game_running then begin
        (* affichage du fond*)
        let origin = Vector2.create 0. 0. in
        let menu_source = Rectangle.create 0. 0. (4608.) (2592.) in
        let menu_dest_rect = Rectangle.create 0. 0. (float_of_int resolution_X) (float_of_int resolution_Y) in
        draw_texture_pro bg_texture menu_source menu_dest_rect origin 0. Color.white;

        (* affichage de la porte*)
        draw_texture_pro porte_texture (Rectangle.create 0. 0. 227. 367.) (Rectangle.create 1450. 727. 150. 200.) (Vector2.create 0. 0.) 0. Color.white; 
        
        (* affichage du joueur *)
        let player = entities.player in
        let f = 
          if is_using_grap player && (get_frame player == 0) then 0.
          else
          if (player.airborn && get_frame player<30) then 245.
          else if (fst player.vector_velocity = 0. && get_frame player<30) then 210.
          else let frame = get_frame player in match frame with
                |_ when frame<5 -> 0.
                |_ when frame<10 -> 35.
                |_ when frame<15 -> 70.
                |_ when frame<20 -> 105.
                |_ when frame<25 -> 140.
                |_ when frame<30 -> 175.
                |_ when frame<40 -> 280.
                |_ when frame<50 -> 355.
                |_ -> 210.
        in
        let t = if get_frame player>30 then 75. else 35. in
        let source_rect = Rectangle.create f 0. (if player.facing_right then (t) else -. (t)) (50.) in
        let dest_rect = Rectangle.create (if get_frame player>30 && (not player.facing_right) then fst player.pos -. player.width else fst player.pos) (float_of_int(resolution_Y) -. (snd player.pos)) ((if get_frame player>30 then (player.width)*.2. else player.width)) (player.height) in 
        let origin = Vector2.create 0. 0. in
        draw_texture_pro sprite_texture source_rect dest_rect origin 0. Color.white;  

        (* debug *)
        (* draw_text (string_of_int(get_frame player)) 100 100 30 Color.red; *)

        (* affichage du grapin*)
        if (is_using_grap player) then draw_line (int_of_float(fst player.pos +. (player.width /. 2.))) (resolution_Y - int_of_float(snd player.pos)) (int_of_float(fst (get_pos_grap player))) (resolution_Y - int_of_float(snd (get_pos_grap player))) Color.black; 
        
        (* affichage du carburant *)
        draw_texture_pro carburant_texture (Rectangle.create 0. 0. 40. 40.) (Rectangle.create 30. 10. 40. 40.) (Vector2.create 0. 0.) 0. Color.white; 
        draw_rectangle 80 20 (2*(get_carburant player)) 20 Color.skyblue;

        (* affichage de la vie *)
        draw_texture_pro vie_texture (Rectangle.create 0. 0. 40. 40.) (Rectangle.create 30. 50. 40. 40.) (Vector2.create 0. 0.) 0. Color.white; 
        draw_rectangle 80 60 (2*(get_health_point player)) 20 Color.green;

        (* affichage ennemis *)
        let source_rect = Rectangle.create 0. 0. 350. 350. in
        List.iter2 (fun (e,_,_) -> fun t ->
          let enemy_dest_rect = Rectangle.create (fst e.pos) (float_of_int(resolution_Y) -. (snd e.pos)) (e.width) (e.height) in
        draw_texture_pro t source_rect enemy_dest_rect origin 0. Color.white) entities.ennemis enemy_textures ;
        
        (* affichage plateforme *)
        List.iter (fun p -> draw_rectangle p.platform_x (resolution_Y - p.platform_y) p.platform_width p.platform_height Color.darkbrown) entities.plateforme_list;
      end else begin
        (* affichage du fond menu *)
        let origin = Vector2.create 0. 0. in
        let menu_source = Rectangle.create 0. 0. (1600.) (900.) in
        let menu_dest_rect = Rectangle.create 0. 0. (float_of_int resolution_X) (float_of_int resolution_Y) in
        draw_texture_pro menu_texture menu_source menu_dest_rect origin 0. Color.white;
        if !is_start_visible then 
          draw_text "Appuyez sur entrée pour commencer" 350 400 50 Color.red;
      
      end;
      end_drawing (); 
    in
    draw_game entities;
    
    (* passage au niveau suivant *)
    if (
      1510. >= (fst entities.player.pos) && 1510. <= (fst entities.player.pos +. entities.player.width) &&
      25. >= (snd entities.player.pos-. entities.player.height) && 25. <= (snd entities.player.pos)
      )
    then
    let level = if level=4 then 1 else level+1 in
    let levelf =
      match level with
      |1 -> "../resources/level/level1.json"
      |2 -> "../resources/level/level2.json"
      |3 -> "../resources/level/level3.json"
      |4 -> "../resources/level/level4.json"
      |_ -> "../resources/level/level1.json"
    in
    let parsed_list = parse_json levelf in 
    let pennemis = List.init (List.length (snd parsed_list)) (fun i ->
      let l = List.nth (snd parsed_list) i in
      match l with
      |s::xmin::xmax::y::[] -> (match (s,xmin,xmax,y) with
        |(`String si, `Float xmini, `Float xmaxi, `Float yi) -> (create_character 1 si xmini yi 150. 150., xmini, xmaxi)
        | _ -> failwith "wrong json format"
        )
      | _ -> failwith "wrong json format"
      )
    in
    let enemy_textures = List.init (List.length pennemis) (fun i -> let (e, _, _) = List.nth pennemis i in
      Raylib.load_texture e.sprite) in 
      let p_list = List.init (List.length (fst parsed_list)) (fun i -> 
        let l = List.nth (fst parsed_list) i in
        match l with
        |x::y::w::h::[] -> (match (x,y,w,h) with
        |(`Int xi, `Int yi, `Int wi, `Int hi) -> {platform_x = xi; platform_y = yi; platform_width = wi; platform_height = hi;}
        | _ -> failwith "wrong json format")
        | _ -> failwith "wrong json format"
        ) in
        let bg_texture = 
        match level with
        |1 -> Raylib.load_texture "../resources/PNG/background 1/Preview 1.png"
        |2 -> Raylib.load_texture "../resources/PNG/background 3/Preview 3.png"
        |3 -> Raylib.load_texture "../resources/PNG/background 2/Preview 2.png"
        |4 -> Raylib.load_texture "../resources/PNG/background 4/Preview 4.png"
        |_ -> Raylib.load_texture "../resources/PNG/background 2/Preview 2.png"
        in
         let j =
        let* () = set_pos (0., 100.) in  set_health 100 in
        let (_,joueur) = j entities.player in
      loop menu_texture bg_texture sprite_texture enemy_textures {player = joueur; ennemis = pennemis; plateforme_list = p_list} carburant_texture vie_texture porte_texture level
    
    else
    loop menu_texture bg_texture sprite_texture enemy_textures entities carburant_texture vie_texture porte_texture level

let gameloop () =
  Raylib.init_window resolution_X resolution_Y "L'ATTAQUE DES TITOUAN";
  Raylib.set_target_fps 60;
  let menu_texture, bg_texture, sprite_texture, enemy_textures, entities, carburant_texture, vie_texture, porte_texture = setup () in
  loop menu_texture bg_texture sprite_texture enemy_textures entities carburant_texture vie_texture porte_texture 1
