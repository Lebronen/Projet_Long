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
plateforme_list : plateforme list
}

let check_collision player plateforme =
  let px, py = player.pos in
  px +. player.sprite_width > float_of_int plateforme.platform_x &&
  px < float_of_int plateforme.platform_x +. float_of_int plateforme.platform_width &&
  py +. player.sprite_height > float_of_int plateforme.platform_y &&
  py < float_of_int plateforme.platform_y +. float_of_int plateforme.platform_height

  (* let player_on_platform player =
    List.filter (check_collision player) p_list *)

  let setup () =
    Raylib.init_window 1200 650 "L'ATTAQUE DES TITOUAN";
    Raylib.set_target_fps 60;

    let menu_texture = Raylib.load_texture "../resources/attaque-titans.png" in
    (* let game_texture = Raylib.load_texture "nouvelle-image.png" in *)
    let player = create_personnage "eren" "../resources/eren.gif" 92. 120. in
    
    let sprite_texture = Raylib.load_texture player.sprite in
    (menu_texture, sprite_texture, player)
  
  let start_time = ref (Raylib.get_time ())
  let is_start_visible = ref true
  let is_game_running = ref false
  
  (* Dimensions *)
  (* let screen_width = 1200
  let screen_height = 650 *)
  let sprite_width = 130
  let sprite_height = 92

  let plateforme = {
    platform_x = 500;
    platform_y = 500;
    platform_width = 400;
    platform_height = 20;
  }

  let plateforme_2 = {
  platform_x = 100;
  platform_y = 400;
  platform_width = 200;
  platform_height = 20;
}

let p_list = [plateforme; plateforme_2]

  let rec loop menu_texture sprite_texture player plateforme=
    if Raylib.window_should_close () then (
      Raylib.unload_texture menu_texture;
      Raylib.unload_texture sprite_texture;
      Raylib.close_window ()
    )
    else
      let open Raylib in
  
      (* Mise à jour du menu *)
      if not !is_game_running then begin
        let current_time = get_time () in
        if current_time -. !start_time >= 0.7 then begin
          is_start_visible := not !is_start_visible;
          start_time := current_time
        end;
  
        if is_key_pressed Key.Enter then is_game_running := true;
  
        let mouse_pos = get_mouse_position () in
        if is_mouse_button_pressed MouseButton.Left then
          if Vector2.x mouse_pos >= 500. && Vector2.x mouse_pos <= 700. &&
             Vector2.y mouse_pos >= 300. && Vector2.y mouse_pos <= 350. then
            is_game_running := true
      end;
  
      let player = 
      (* Mise à jour du jeu *)
      if !is_game_running then
        let player =

        match (is_key_down Key.Right, is_key_down Key.Left) with
        | true, false -> if fst player.vector_velocity < 12. then vel player (4.,0.) else player
        | false, true -> if fst player.vector_velocity > -12. then vel player (-4.,0.) else player
        | _, _ -> if not player.is_jumping then vel player (-.(fst player.vector_velocity), 0.) else player
        in

        let player = vel player (0., 1.) in 
        (* (650.0 -. 92.0) est la valeur a laquel le personnage touche le sol car les coordonées du perso sont en haut a gauche du sprite *)
        let player = if (snd player.pos >= (650.0 -. 92.0)) || List.exists (check_collision player ) p_list
          then vel (jump player false) (0., -.(snd player.vector_velocity))
        else player in 

        let player = if is_key_down Key.Up && not player.is_jumping then vel (jump player true) (0., -30.)
        else player in

        let player = deplacer player 
        in



        player
        else player in

        

        (* Dessin *)
        let draw_game player = 
(
        begin_drawing ();
        clear_background Color.raywhite;
  
      if !is_game_running then begin
        let source_rect = Rectangle.create 0. 0. (if player.facing_right then float_of_int sprite_width else -. (float_of_int sprite_width)) (float_of_int sprite_height) in
        let dest_rect = Rectangle.create (fst player.pos) (snd player.pos) (player.sprite_width) (player.sprite_height) in
        let origin = Vector2.create 0. 0. in
        draw_texture_pro sprite_texture source_rect dest_rect origin 0. Color.white;
        for i = 0 to List.length p_list - 1 do
          let p = List.nth p_list i in
          draw_rectangle p.platform_x p.platform_y p.platform_width p.platform_height Color.black;
        done
        (* Dessiner la plateforme *)
        (* draw_rectangle platform_x platform_y platform_width platform_height Color.darkgray; *)
      end else begin
        draw_texture menu_texture 0 0 Color.white;
        draw_text "Bienvenue dans l'attaque des Titouan!" 134 104 50 Color.black;
        draw_text "Bienvenue dans l'attaque des Titouan!" 130 100 50 Color.red;
  
        if !is_start_visible then begin
          draw_text "START" 504 304 50 Color.black;
          draw_text "START" 500 300 50 Color.red;
        end
      end;
  
      end_drawing ();
)
    in draw_game player;
      loop menu_texture sprite_texture player plateforme
  
  let gameloop () =
    let menu_texture, sprite_texture, player = setup () in
    loop menu_texture sprite_texture player plateforme
  
  