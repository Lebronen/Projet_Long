open Joueur

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
  player : joueur;
  ennemis : joueur list;
  plateforme_list : plateforme list;
}

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

let check_plateforme player plateform = 
  ((snd player.vector_velocity +. snd player.pos +. player.sprite_height) > float_of_int plateform.platform_y
    && (snd player.pos +. player.sprite_height) <= float_of_int plateform.platform_y
    && (fst player.pos +. player.sprite_width) > float_of_int plateform.platform_x
    && (fst player.pos) < (float_of_int plateform.platform_x +. float_of_int plateform.platform_width))

let is_on_plateforme player p_list =
  List.exists (check_plateforme player) p_list

let wich_plateforme player p_list =
  List.filter (check_plateforme player) p_list

let setup () =
  Raylib.init_window resolution_X resolution_Y "L'ATTAQUE DES TITOUAN";
  Raylib.set_target_fps 60;

  let menu_texture = Raylib.load_texture "../resources/attaque-titans.png" in
  let player = create_personnage "eren" "../resources/red.png" 100. 100. 50. 800. in
  let enemy = create_personnage "ennemi" "../resources/blue.png" 100. 100. 1200. 650. in

  let sprite_texture = Raylib.load_texture player.sprite in
  let enemy_texture = Raylib.load_texture enemy.sprite in

  let plateforme = { platform_x = 500; platform_y = 500; platform_width = 400; platform_height = 20; } in
  let plateforme_2 = { platform_x = 100; platform_y = 400; platform_width = 200; platform_height = 20; } in
  let plateforme_3 = { platform_x = 600; platform_y = 200; platform_width = 300; platform_height = 20; } in
  let plateforme_4 = { platform_x = 900; platform_y = 700; platform_width = 400; platform_height = 20; } in
  let p_list = [plateforme; plateforme_2; plateforme_3; plateforme_4] in
  let entities = { player; ennemis = [enemy]; plateforme_list = p_list } in
  (menu_texture, sprite_texture, enemy_texture, entities)

let start_time = ref (Raylib.get_time ())
let is_start_visible = ref true
let is_game_running = ref false

let rec loop menu_texture sprite_texture enemy_texture entities =
  if Raylib.window_should_close () then (
    Raylib.unload_texture menu_texture;
    Raylib.unload_texture sprite_texture;
    Raylib.unload_texture enemy_texture;
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
    let entities = 
    let player = entities.player in
    let joueur = 
       (if !is_game_running then
        let player = vel player (0., 1.) in

        let player =
          match (is_key_down Key.Right, is_key_down Key.Left) with
          | true, false -> if fst player.vector_velocity < 12. then vel player (4.,0.) else player
          | false, true -> if fst player.vector_velocity > -12. then vel player (-4.,0.) else player
          | _, _ -> if not player.is_jumping then vel player (-.(fst player.vector_velocity), 0.) else player
        in
        let player = if ((snd player.vector_velocity +. snd player.pos +. player.sprite_height) > float_of_int resolution_Y)
          then vel (jump player false) (0., -.(snd player.vector_velocity -. (float_of_int resolution_Y -. (snd player.pos +. player.sprite_height))))
          else player
        in
        let player = if is_on_plateforme player entities.plateforme_list && not player.grap.using
          then let p = List.nth (wich_plateforme player entities.plateforme_list) 0 in vel (jump player false) (0., -.(snd player.vector_velocity -. (float_of_int p.platform_y -. (snd player.pos +. player.sprite_height))))
          else player
        in
        let player = if is_key_down Key.Space then
          let (vx', vy') = pendule (fst player.pos) (snd player.pos) (fst player.grap.pos) (snd player.grap.pos) (fst player.vector_velocity) (snd player.vector_velocity)
          in
          let player = 
            if player.grap.using then jump player true
            else if player.facing_right 
              then grapin (jump player true) true (fst player.pos +. 200., snd player.pos -. 150.)
              else grapin (jump player true) true (fst player.pos -. 150., snd player.pos -. 150.)
            in
          vel player (-.fst player.vector_velocity +. vx',-.snd player.vector_velocity +. vy')
          else grapin player false player.grap.pos in
        let player = if is_key_down Key.Up && not player.is_jumping then vel (jump player true) (0., -20.)
        else player in
        let player = deplacer player in player else player)
      in {player = joueur; ennemis = entities.ennemis; plateforme_list = entities.plateforme_list} in

    let draw_game entities = 
      begin_drawing (); 
      clear_background Color.raywhite;
      if !is_game_running then begin
        let player = entities.player in
        let source_rect = Rectangle.create 0. 0. (if player.facing_right then player.sprite_width else -. (player.sprite_width)) (player.sprite_height) in
        let dest_rect = Rectangle.create (fst player.pos) (snd player.pos) (player.sprite_width) (player.sprite_height) in 
        let origin = Vector2.create 0. 0. in
        draw_texture_pro sprite_texture source_rect dest_rect origin 0. Color.white;
        
        let enemy = List.hd entities.ennemis in
        let enemy_dest_rect = Rectangle.create (1000.) (550.) (enemy.sprite_width) (enemy.sprite_height) in
        draw_texture_pro enemy_texture source_rect enemy_dest_rect origin 0. Color.white;
        
        List.iter (fun p -> draw_rectangle p.platform_x p.platform_y p.platform_width p.platform_height Color.black) entities.plateforme_list;
      end else begin
        (* let origin = Vector2.create 0. 0. in
        let menu_source = Rectangle.create 0. 0. (1200.) (673.) in
        let menu_dest_rect = Rectangle.create 0. 0. (float_of_int resolution_X) (float_of_int resolution_Y) in
        draw_texture_pro menu_texture menu_source menu_dest_rect origin 0. Color.white; *)
        draw_text "Bienvenue dans l'attaque des Titouan!" 300 200 50 Color.red;
        if !is_start_visible then draw_text "START" 690 400 50 Color.red;
      end;
      end_drawing (); 
    in
    draw_game entities;
    loop menu_texture sprite_texture enemy_texture entities 

let gameloop () =
  let menu_texture, sprite_texture, enemy_texture, entities = setup () in
  loop menu_texture sprite_texture enemy_texture entities