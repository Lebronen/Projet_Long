let setup () =
  Raylib.init_window 1200 650 "L'ATTAQUE DES TITOUAN";
  Raylib.set_target_fps 60;
  let menu_texture = Raylib.load_texture "attaque-titans.png" in
  let game_texture = Raylib.load_texture "nouvelle-image.png" in
  let sprite_texture = Raylib.load_texture "eren.gif" in
  (menu_texture, game_texture, sprite_texture)

(* Temps initial pour gérer l'alternance *)
let start_time = ref (Raylib.get_time ())
(* État pour savoir si le message START est visible *)
let is_start_visible = ref true
(* État pour savoir si le jeu est en cours (menu terminé) *)
let is_game_running = ref false
(* Position initiale du sprite *)
let sprite_position = ref (Raylib.Vector2.create 600. 325.)

let rec loop menu_texture game_texture sprite_texture =
  if Raylib.window_should_close () then (
    Raylib.unload_texture menu_texture; (* Libérer les ressources de la texture du menu *)
    Raylib.unload_texture game_texture; (* Libérer les ressources de la texture du jeu *)
    Raylib.unload_texture sprite_texture; (* Libérer les ressources de la texture du sprite *)
    Raylib.close_window ()
  )
  else
    let open Raylib in

    (* Mise à jour du clignotement du message START si dans le menu *)
    if not !is_game_running then begin
      let current_time = get_time () in
      if current_time -. !start_time >= 0.7 then begin
        is_start_visible := not !is_start_visible; (* Alterner la visibilité *)
        start_time := current_time (* Réinitialiser le temps de départ *)
      end;

      (* Transition vers le jeu si la touche Entrée est pressée *)
      if is_key_pressed Key.Enter then
        is_game_running := true;

      (* Transition vers le jeu si "START" est cliqué *)
      let mouse_pos = get_mouse_position () in
      let mouse_x = Vector2.x mouse_pos in
      let mouse_y = Vector2.y mouse_pos in
      if is_mouse_button_pressed MouseButton.Left then
        if mouse_x >= 500. && mouse_x <= 700. && mouse_y >= 300. && mouse_y <= 350. then
          is_game_running := true
    end;

    (* Mise à jour de la position du sprite si le jeu est en cours *)
    if !is_game_running then begin
      if is_key_down Key.Right then
        sprite_position := Vector2.create ((Vector2.x !sprite_position) +. 5.) (Vector2.y !sprite_position);
      if is_key_down Key.Left then
        sprite_position := Vector2.create ((Vector2.x !sprite_position) -. 5.) (Vector2.y !sprite_position);
      if is_key_down Key.Up then
        sprite_position := Vector2.create (Vector2.x !sprite_position) ((Vector2.y !sprite_position) -. 5.);
      if is_key_down Key.Down then
        sprite_position := Vector2.create (Vector2.x !sprite_position) ((Vector2.y !sprite_position) +. 5.)
    end;

    begin_drawing ();
    clear_background Color.raywhite;

    if !is_game_running then begin
      (* Afficher l'image du jeu *)
      draw_texture game_texture 0 0 Color.white;
      (* Afficher le sprite *)
      draw_texture_v sprite_texture !sprite_position Color.white
    end else begin
      (* Afficher le menu *)
      draw_texture menu_texture 0 0 Color.white;

      (* Dessiner le message "Bienvenue" (toujours visible) *)
      draw_text "Bienvenue dans l'attaque des Titouan!" 134 104 50 Color.black;
      draw_text "Bienvenue dans l'attaque des Titouan!" 130 100 50 Color.red;

      (* Dessiner le message "START" (clignotant) *)
      if !is_start_visible then begin
        draw_text "START" 504 304 50 Color.black;
        draw_text "START" 500 300 50 Color.red;
      end
    end;

    end_drawing ();
    loop menu_texture game_texture sprite_texture

let () =
  let menu_texture, game_texture, sprite_texture = setup () in
  loop menu_texture game_texture sprite_texture
