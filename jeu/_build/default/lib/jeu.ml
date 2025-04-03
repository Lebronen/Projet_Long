open Yojson
open Ennemi
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
  player : Joueur.t;
  ennemis : ennemi;
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

    (* let check_plateforme player plateform = 
      ((snd player.vector_velocity +. snd player.pos -. player.height) < float_of_int plateform.platform_y
        && (snd player.pos -. player.height) >= float_of_int plateform.platform_y
        && (fst player.pos +. player.width -. 25.) > float_of_int plateform.platform_x
        && (fst player.pos +. 25.) < (float_of_int plateform.platform_x +. float_of_int plateform.platform_width))

    let is_on_plateforme player p_list =
      List.exists (check_plateforme player) p_list

    let wich_plateforme player p_list =
      List.filter (check_plateforme player) p_list


    let check_plateforme_r player plateform =
      ((fst player.vector_velocity +. fst player.pos +. player.width) > float_of_int plateform.platform_x
        && (fst player.pos +. player.width) <= float_of_int plateform.platform_x
        && (snd player.pos -. player.height) < float_of_int plateform.platform_y
        && (snd player.pos) > (float_of_int plateform.platform_y -. float_of_int plateform.platform_height))

    let is_on_plateforme_r player p_list =
    List.exists (check_plateforme_r player) p_list

    let wich_plateforme_r player p_list =
    List.filter (check_plateforme_r player) p_list

    let check_plateforme_l player plateform =
      ((fst player.vector_velocity +. fst player.pos) < ((float_of_int plateform.platform_x) +. (float_of_int plateform.platform_width))
        && (fst player.pos) >= (float_of_int plateform.platform_x +. (float_of_int plateform.platform_width))
        && (snd player.pos -. player.height) < float_of_int plateform.platform_y
        && (snd player.pos) > (float_of_int plateform.platform_y -. float_of_int plateform.platform_height))

    let is_on_plateforme_l player p_list =
    List.exists (check_plateforme_l player) p_list

    let wich_plateforme_l player p_list =
    List.filter (check_plateforme_l player) p_list *)

    
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

    let is_on_plateforme direction (player: Joueur.t) p_list =
      List.exists (check_plateforme direction player.character.pos player.character.vector_velocity player.character.height player.character.width) p_list

    let is_on_plateforme_ennemi direction (enemy: ennemi) p_list =
      List.exists (check_plateforme direction enemy.pos enemy.vector_velocity enemy.height enemy.width) p_list

    let wich_plateforme direction (player: Joueur.t) p_list =
      List.filter (check_plateforme direction player.character.pos player.character.vector_velocity player.character.height player.character.width) p_list

    let wich_plateforme_ennemi direction (enemy: ennemi) p_list =
      List.filter (check_plateforme direction enemy.pos enemy.vector_velocity enemy.height enemy.width) p_list
   

let setup () =
  Raylib.init_window resolution_X resolution_Y "L'ATTAQUE DES TITOUAN";
  Raylib.set_target_fps 60;

  let menu_texture = Raylib.load_texture "../resources/PNG/background 2/Preview 2.png" in
  let player = create_personnage (200. , 200.) "../resources/spritesheetcourse.png" 100. 70. in
  (* let enemy = create_personnage "ennemi" "../resources/blue.png" 100. 100. 1200. 650. in *)
  let enemy = create_ennemi "ennemi" "../resources/blue.png" 100. 100. 1174. 650. in


  let sprite_texture = Raylib.load_texture player.character.sprite in
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
    let entities = (
    let ennemis = entities.ennemis in
    let ennemi = 
      (if !is_game_running then
      let ennemis = vele ennemis (0., -1.) in
      (* gestion des colisions avec le sol *)
      let ennemis = if ((snd ennemis.vector_velocity +. snd ennemis.pos -. ennemis.height) < 0.)
        then vele ennemis (0., -.(snd ennemis.vector_velocity +. (snd ennemis.pos -. ennemis.height)))
        else ennemis
      in
      (* gestion des colisions avec les plateforme *)
      let ennemis = if is_on_plateforme_ennemi Below ennemis entities.plateforme_list
        then let p = List.nth (wich_plateforme_ennemi Below ennemis entities.plateforme_list) 0 in vele ennemis (0., -.(snd ennemis.vector_velocity -. (float_of_int p.platform_y -. (snd ennemis.pos -. ennemis.height))))
        else ennemis
      in
      (* gestion colision droite *)
      let ennemis = if is_on_plateforme_ennemi Right ennemis entities.plateforme_list
        then let p = List.nth (wich_plateforme_ennemi Right ennemis entities.plateforme_list) 0 in vele ennemis (-.(fst ennemis.vector_velocity -. (float_of_int p.platform_x -. (fst ennemis.pos +. ennemis.width))), 0.)
        else ennemis
      in
      (* gestion colision gauche *)
      let ennemis = if is_on_plateforme_ennemi Left ennemis entities.plateforme_list
        then let p = List.nth (wich_plateforme_ennemi Left ennemis entities.plateforme_list) 0 in vele ennemis (-.(fst ennemis.vector_velocity -. ((float_of_int p.platform_x +. float_of_int p.platform_width) -. (fst ennemis.pos))), 0.)
        else ennemis
      in
      let ennemis = deplace ennemis in
      ennemis
    else ennemis) in
    let player = entities.player in
    let joueur = 
       (if !is_game_running then
        (* gravité *)
        let player = player.vel (0.) (-1.) in
        (* déplacement latéral *)
        let player =
          match (is_key_down Key.Right, is_key_down Key.Left) with
          | true, false -> if fst player.vector_velocity < 8. then vel player (4.,0.) else player
          | false, true -> if fst player.vector_velocity > -8. then vel player (-4.,0.) else player
          | _, _ -> if not player.character.airborn then vel player (-.(fst player.vector_velocity), 0.) else player
        in
        (* gestion des colisions avec le sol *)
        let player = if ((snd player.vector_velocity +. snd player.pos -. player.height) < 0.)
          then vel (airb player false) (0., -.(snd player.vector_velocity +. (snd player.pos -. player.height)))
          else player
        in
        (* gestion des colisions avec les plateforme *)
        let player = if is_on_plateforme Below player entities.plateforme_list && not player.grap.using
          then let p = List.nth (wich_plateforme Below player entities.plateforme_list) 0 in vel (airb player false) (0., -.(snd player.vector_velocity -. (float_of_int p.platform_y -. (snd player.pos -. player.height))))
          else player
        in
        (* gestion colision droite *)
        let player = if is_on_plateforme Right player entities.plateforme_list
          then let p = List.nth (wich_plateforme Right player entities.plateforme_list) 0 in vel player (-.(fst player.vector_velocity -. (float_of_int p.platform_x -. (fst player.pos +. player.width))), 0.)
          else player
        in
        (* gestion colision gauche *)
        let player = if is_on_plateforme Left player entities.plateforme_list
          then let p = List.nth (wich_plateforme Left player entities.plateforme_list) 0 in vel player (-.(fst player.vector_velocity -. ((float_of_int p.platform_x +. float_of_int p.platform_width) -. (fst player.pos))), 0.)
          else player
        in
        
        (* gestion du grappin *)
        let player = if is_key_down Key.Space then
          let (vx', vy') = pendule (fst player.pos +. (player.width /. 2.)) (snd player.pos) (fst player.grap.pos) (snd player.grap.pos) (fst player.vector_velocity) (snd player.vector_velocity)
          in
          let player = 
            if player.grap.using then (airb player true)
            else if player.facing_right 
              then grapin (airb player true) true (fst player.pos +. 250. +. (player.width /. 2.), snd player.pos +. 150.)
              else grapin (airb player true) true (fst player.pos -. 250. +. (player.width /. 2.), snd player.pos +. 150.)
            in  
          vel player (-.fst player.vector_velocity +. vx',-.snd player.vector_velocity +. vy')
          else grapin player false player.grap.pos in
        (* saut *)
        let player = if is_key_down Key.Up && not player.airborn then vel (airb player true) (0., 20.)
        else player in
        (* mise a jours des positions *)
        let player = deplacer player in player else player) in


        {player = joueur; ennemis = ennemi; plateforme_list = entities.plateforme_list}
      ) in

    
    let draw_game entities = 
      begin_drawing (); 
      clear_background Color.white;
      
      if !is_game_running then begin
        let origin = Vector2.create 0. 0. in
        let menu_source = Rectangle.create 0. 0. (4608.) (2592.) in
        let menu_dest_rect = Rectangle.create 0. 0. (float_of_int resolution_X) (float_of_int resolution_Y) in
        draw_texture_pro menu_texture menu_source menu_dest_rect origin 0. Color.white;

        let player = entities.player in
        
        let f = 
          match player.character.vector_velocity with
          |(0.,_) -> 0.
          |_ -> match frame with
                |_ when frame<5 -> 0.
                |_ when frame<10 -> 35.
                |_ when frame<15 -> 70.
                |_ when frame<20 -> 105.
                |_ when frame<25 -> 140.
                |_ -> 175.
      in
        let source_rect = Rectangle.create f 0. (if player.character.facing_right then (35.) else -. (35.)) (50.) in
        let dest_rect = Rectangle.create (fst player.character.pos) (float_of_int(resolution_Y) -. (snd player.character.pos)) (player.character.width) (player.character.height) in 
        let origin = Vector2.create 0. 0. in
        draw_texture_pro sprite_texture source_rect dest_rect origin 0. Color.white;
        
        if (player.grap.using) then draw_line (int_of_float(fst player.character.pos +. (player.character.width /. 2.))) (resolution_Y - int_of_float(snd player.character.pos)) (int_of_float(fst player.grap.pos)) (resolution_Y - int_of_float(snd player.grap.pos)) Color.black; 

        let enemy = entities.ennemis in
        let enemy_dest_rect = Rectangle.create (fst enemy.pos) (float_of_int(resolution_Y) -. (snd enemy.pos)) (enemy.width) (enemy.height) in
        draw_texture_pro enemy_texture source_rect enemy_dest_rect origin 0. Color.white;
        
        draw_rectangle 50 50 (3*player.health_point) 20 Color.green;

        List.iter (fun p -> draw_rectangle p.platform_x (resolution_Y - p.platform_y) p.platform_width p.platform_height Color.brown) entities.plateforme_list;
      end else begin

        (* draw_text "Bienvenue dans l'attaque des Titouan!" 300 200 50 Color.red; *)
        if !is_start_visible then draw_text "Appuyez sur entrée pour commencer" 350 400 50 Color.red;
      end;
      end_drawing (); 
    in
    draw_game entities;
    loop menu_texture sprite_texture enemy_texture entities (if (frame==30) then 0 else (frame+1))

let gameloop () =
  let menu_texture, sprite_texture, enemy_texture, entities = setup () in
  loop menu_texture sprite_texture enemy_texture entities 0