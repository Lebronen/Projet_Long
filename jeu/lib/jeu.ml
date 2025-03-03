open Joueur


type settings = {
  starttime : float;
  isstartvisible : bool;
  is_game_running : bool;
}
type plateforme = {
  platform_x : float;
  platform_y : float;
  platform_width : float;
  platform_height : float
}

type entities = {
player : joueur;
plateforme_list : plateforme list
}


  let setup () =
    Raylib.init_window 1200 650 "L'ATTAQUE DES TITOUAN";
    Raylib.set_target_fps 60;

    let menu_texture = Raylib.load_texture "../resources/attaque-titans.png" in
    (* let game_texture = Raylib.load_texture "nouvelle-image.png" in *)
    let player = create_personnage "eren" "../resources/eren.gif" 92. 50. in
    let sprite_texture = Raylib.load_texture player.sprite in
    (menu_texture, sprite_texture, player)
  
  let start_time = ref (Raylib.get_time ())
  let is_start_visible = ref true
  let is_game_running = ref false
  
  (* Dimensions *)
  (* let screen_width = 1200
  let screen_height = 650 *)
  let sprite_width = 50
  let sprite_height = 92

  
  
  
  let rec loop menu_texture sprite_texture player =
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
  
      (* Mise à jour du jeu *)
      if !is_game_running then
        let player =
          if is_key_down Key.Right then if player.is_moving_right then player else vel (moving_right player true) (5.,0.)
        else if is_key_released Key.Right then vel (moving_right player false) (-5., 0.) 
          else if is_key_down Key.Left then if player.is_moving_left then player else vel (moving_left player true) (-5.,0.)
        else if is_key_released Key.Left then vel (moving_left player false) (5., 0.)
          else player
        in
      let player =
        if is_key_pressed Key.Up && not player.is_jumping then vel player (0., -15.)
        else player
      in
  
        let player = if player.is_jumping then vel player (0., 5.)
        else player
    in  
        (* Vérifier si le sprite touche le sol *)
        (* if new_y >= float_of_int screen_height -. player.sprite_height then begin *)
         let player = vel player (0., -.(snd player.vector_velocity))
         in
        (* Vérifier s'il atterrit sur la plateforme (seulement s'il ne veut pas traverser) *)
        (* else if !velocity_y > 0. && not !falling_through_platform &&
                new_y +. float_of_int sprite_height >= float_of_int platform_y &&
                new_y +. float_of_int sprite_height <= float_of_int (platform_y + platform_height) &&
                new_x +. float_of_int sprite_width > float_of_int platform_x &&
                new_x < float_of_int (platform_x + platform_width) then begin
          sprite_position := Vector2.create new_x (float_of_int (platform_y - sprite_height));
          is_jumping := false;
          velocity_y := 0.
        end 
        (* Si le sprite tombe sous la plateforme, il peut y atterrir à nouveau *)
        else if new_y > float_of_int (platform_y + platform_height) then  
        (* Mise à jour de la position *)
        sprite_position := Vector2.create new_x new_y;
      end;
  *)
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
      loop menu_texture sprite_texture player
  
  let gameloop () =
    let menu_texture, sprite_texture, player = setup () in
    loop menu_texture sprite_texture player
  
  