let setup () =
  Raylib.init_window 1200 650 "L'ATTAQUE DES TITOUAN";
  Raylib.set_target_fps 60;
  Raylib.load_texture "attaque-titans.png" (* Charger la texture après l'initialisation *)

(* Temps initial pour gérer l'alternance *)
let start_time = ref (Raylib.get_time ())
(* État pour savoir si le message START est visible *)
let is_start_visible = ref true

let rec loop texture =
  if Raylib.window_should_close () then (
    Raylib.unload_texture texture; (* Libérer les ressources de la texture *)
    Raylib.close_window ()
  )
  else
    let open Raylib in

    (* Mise à jour du clignotement du message START *)
    let current_time = get_time () in
    if current_time -. !start_time >= 0.7 then begin
      is_start_visible := not !is_start_visible; (* Alterner la visibilité *)
      start_time := current_time (* Réinitialiser le temps de départ *)
    end;

    begin_drawing ();
    clear_background Color.raywhite;

    (* Dessiner l'image *)
    draw_texture texture 0 0 Color.white;

    (* Dessiner le message "Bienvenue" (toujours visible) *)
    draw_text "Bienvenue dans l'attaque des Titouan!" 134 104 50 Color.black;
    draw_text "Bienvenue dans l'attaque des Titouan!" 130 100 50 Color.red;

    (* Dessiner le message "START" (clignotant) *)
    if !is_start_visible then begin
      draw_text "START" 504 304 50 Color.black;
      draw_text "START" 500 300 50 Color.red;
    end;

    end_drawing ();
    loop texture

let () =
  let texture = setup () in
  loop texture
