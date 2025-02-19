open Joueur

let eren = create_personnage "eren" "../resources/eren.gif"

type settings
  let setup () =
    Raylib.init_window 1200 650 "L'ATTAQUE DES TITOUAN";
    Raylib.set_target_fps 60;

    let sprite_texture = drawme eren in
    let menu_texture = Raylib.load_texture "../resources/attaque-titans.png" in
    let game_texture = Raylib.load_texture "nouvelle-image.png" in
    (menu_texture, game_texture, sprite_texture)
  
  let start_time = ref (Raylib.get_time ())
  let is_start_visible = ref true
  let is_game_running = ref false
  
  (* Position et physique du sprite *)
  let sprite_position = ref (Raylib.Vector2.create 50. (650.0 -. 92.0))
  let velocity_y = ref 0.
  let is_jumping = ref false
  let falling_through_platform = ref false  (* Permet de tomber de la plateforme *)
  
  (* Dimensions *)
  let screen_width = 1200
  let screen_height = 650
  let sprite_width = 135
  let sprite_height = 92
  let facing_right = ref true
  
  (* Plateforme *)
  let platform_x = 300
  let platform_y = 500
  let platform_width = 200
  let platform_height = 20
  

  let rec loop menu_texture game_texture sprite_texture =
    if Raylib.window_should_close () then (
      Raylib.unload_texture menu_texture;
      Raylib.unload_texture game_texture;
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
      if !is_game_running then begin
        let new_x =
          if is_key_down Key.Right then (facing_right := true; min (Vector2.x !sprite_position +. 5.) (float_of_int (screen_width - sprite_width)))
          else if is_key_down Key.Left then (facing_right := false; max (Vector2.x !sprite_position -. 5.) 0.)
          else Vector2.x !sprite_position
        in
  
        (* Permet de tomber de la plateforme en appuyant sur la flèche du bas *)
        if is_key_pressed Key.Down &&
           Vector2.y !sprite_position +. float_of_int sprite_height = float_of_int platform_y &&
           new_x +. float_of_int sprite_width > float_of_int platform_x &&
           new_x < float_of_int (platform_x + platform_width) then begin
          falling_through_platform := true;
          is_jumping := true;
          velocity_y := 5.;  (* Donne une petite impulsion vers le bas *)
        end;
  
        if is_key_pressed Key.Up && not !is_jumping then begin
          is_jumping := true;
          velocity_y := -15.
        end;
  
        (* Gestion du saut et de la gravité *)
        let new_y = Vector2.y !sprite_position +. !velocity_y in
        velocity_y := !velocity_y +. 0.5;
  
        (* Vérifier si le sprite touche le sol *)
        if new_y >= float_of_int screen_height -. float_of_int sprite_height then begin
          sprite_position := Vector2.create new_x (float_of_int (screen_height - sprite_height));
          is_jumping := false;
          velocity_y := 0.;
          falling_through_platform := false; (* Réinitialiser pour qu'il puisse atterrir sur la plateforme à nouveau *)
        end
        (* Vérifier s'il atterrit sur la plateforme (seulement s'il ne veut pas traverser) *)
        else if !velocity_y > 0. && not !falling_through_platform &&
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
          falling_through_platform := false;
  
        (* Mise à jour de la position *)
        sprite_position := Vector2.create new_x new_y;
      end;
  
      (* Dessin *)
      begin_drawing ();
      clear_background Color.raywhite;
  
      if !is_game_running then begin
        draw_texture game_texture 0 0 Color.white;
        let source_rect = Rectangle.create 0. 0. (if !facing_right then float_of_int sprite_width else -. (float_of_int sprite_width)) (float_of_int sprite_height) in
        let dest_rect = Rectangle.create (Vector2.x !sprite_position) (Vector2.y !sprite_position) (float_of_int sprite_width) (float_of_int sprite_height) in
        let origin = Vector2.create 0. 0. in
        draw_texture_pro sprite_texture source_rect dest_rect origin 0. Color.white;
  
        (* Dessiner la plateforme *)
        draw_rectangle platform_x platform_y platform_width platform_height Color.darkgray;
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
      loop menu_texture game_texture sprite_texture
  
  let gameloop=
    let menu_texture, game_texture, sprite_texture = setup () in
    loop menu_texture game_texture sprite_texture
  
  