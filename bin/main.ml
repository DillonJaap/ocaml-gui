open Gui
open Core
open Raylib

let calculate_scores input_text file_paths =
  file_paths
  |> List.map ~f:(fun elt ->
    (* only test against the last folder name and not the full path *)
    let file_name = elt |> String.split ~on:'/' |> List.rev |> List.hd_exn in
    elt, Scoring.smith_waterman input_text file_name
  )
  |> List.sort ~compare:(fun a b -> Float.compare (snd b) (snd a))
;;

let ranked_list
      ?(x_position = 0)
      ?(y_position = 0)
      ?(font_size = 28)
      ~font
      ?(draw_score = false)
      item_score_tuples
      current_selection
  =
  UI.rectangle
    ~layout:Vertical
    ~x_position
    ~y_position
    (List.mapi item_score_tuples ~f:(fun i score_tuple ->
       UI.rectangle
         ~layout:Horizontal
         ~width:(Fixed 800)
         ~color:(if i = current_selection then Color.skyblue else Color.blank)
         [ UI.text ~padding_all:8 ~font_size ~font (fst score_tuple)
         ; ( if draw_score then
               UI.text
                 ~padding_all:8
                 ~align:Left
                 ~font_size
                 ~font
                 ~text_color:Color.darkpurple
                 (snd score_tuple |> int_of_float |> string_of_int)
             else
               UI.empty ()
           )
         ]
     )
    )
;;

let selection (config : Config.config) =
  (* values that get mutated by user input *)
  let input_text = ref "" in
  let new_text = ref "" in
  let current_selection = ref 0 in

  (* code project directories info *)
  let dirs = SysUtil.find_git_dirs config.code_dir in
  let num_files = List.length dirs in
  let dir_ratio_tuples = ref (calculate_scores !input_text dirs) in

  let rec loop () =
    (* close window and exit loop *)
    if window_should_close () then (
      close_window ();
      exit 0
    );

    (* handle keypresses *)
    begin
      let open Key in
      (* ctrl chords *)
      if Raylib.is_key_down Raylib.Key.Left_control || Raylib.is_key_down Raylib.Key.Right_control
      then (
        match
          get_key_pressed ()
        with
        | A ->
          let dir = List.nth_exn !dir_ratio_tuples !current_selection |> fst in
          let launcher = List.nth_exn config.launchers 1 in

          SysUtil.daemonize
            ~prog:launcher.path
            ~argv:(Array.append (launcher.args |> List.to_array) [| dir |]);
          close_window ();
          exit 0
        | B ->
          let dir = List.nth_exn !dir_ratio_tuples !current_selection |> fst in
          let launcher = List.nth_exn config.launchers 2 in

          SysUtil.daemonize
            ~prog:launcher.path
            ~argv:(Array.append (launcher.args |> List.to_array) [| dir |]);
          close_window ();
          exit 0
        | _ -> ()
      ) else if
          (* regular presses *)
          is_key_pressed Down
        then
        current_selection := min (!current_selection + 1) (num_files - 1)
      else if is_key_pressed Up then
        current_selection := max (!current_selection - 1) 0
      else if is_key_pressed Enter then (
        let dir = List.nth_exn !dir_ratio_tuples !current_selection |> fst in
        let launcher = List.nth_exn config.launchers 0 in

        SysUtil.daemonize
          ~prog:launcher.path
          ~argv:(Array.append (launcher.args |> List.to_array) [| dir |]);
        close_window ();
        exit 0
      )
    end;

    (* draw *)
    begin
      begin_drawing ();
      clear_background Color.lightgray;

      (* text box *)
      (* let rect = Rectangle.create x 5.0 500.0 70.0 in *)
      (* let text, _ = Raygui.text_box rect (pad_input !input_text) true in *)
      (* new_text := text; *)
      let x = (get_screen_width () - 500) / 2 in
      new_text
      := TextBox.draw ~font:config.font ~x_pos:x ~y_pos:5 ~width:500 ~height:70 true
         |> TextArea.IntArray.to_string;

      (* ranked file list *)
      let rl =
        ranked_list
          ~x_position:(x - 100)
          ~y_position:100
          ~font:config.font
          ~font_size:36
          ~draw_score:true
          !dir_ratio_tuples
          !current_selection
        |> UI.calculate_sizes
        |> UI.calculate_positions
      in

      rl |> UI.render;

      end_drawing ()
    end;

    (* only calculate scores if text changed *)
    if not (phys_equal !new_text !input_text) then (
      input_text := !new_text;
      dir_ratio_tuples := calculate_scores !input_text dirs (* current_selection := 0 *)
    );
    ();
    loop ()
  in
  loop ()
;;

let element_list config =
  let rec loop () =
    (* close window and exit loop *)
    if window_should_close () then (
      close_window ();
      exit 0
    );

    begin_drawing ();
    clear_background Color.darkgray;

    (* draw tree *)
    let open UI in
    rectangle
      ~layout:Horizontal
      ~x_position:100
      ~y_position:20
      ~width:Fill
      ~height:Fill
      ~color:Color.darkpurple
      [ rectangle
          ~color:Color.purple
          ~padding_all:10
          [ text ~text_color:Color.gold "this is an item in a list" ]
      ; rectangle
          ~color:Color.purple
          ~padding:{ left = 20; right = 20; top = 20; bottom = 20 }
          [ text ~text_color:Color.gold "this is an item in the list" ]
      ; rectangle
          ~color:Color.purple
          ~padding_all:10
          [ text ~text_color:Color.gold ~padding_all:4 "short text" ]
      ]
    |> calculate_sizes
    |> calculate_positions
    |> render;

    end_drawing ();
    loop ()
  in
  loop ()
;;

let setup () =
  let window_width = 1200 in
  let window_height = 800 in

  init_window window_width window_height "Project Launcher";

  (* theoretically center the window *)
  set_window_position
    ((get_screen_width () - window_width) / 2)
    ((get_screen_height () - window_height) / 2);

  (* target FPS *)
  set_target_fps 60;

  (* Set text size to 24px for ALL controls *)
  Raygui.set_style (Raygui.Control.Default `Text_size) 22;

  (* remove window decoration *)
  set_window_state [ ConfigFlags.Window_undecorated ]
;;

let () =
  setup ();
  let config = Config.initialize () in
  (* element_list config *)
  selection config
;;
