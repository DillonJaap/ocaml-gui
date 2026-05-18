open Gui
open Core
open Raylib

(* TODO package this with the program or use system default? *)

let calculate_scores input_text file_paths =
  file_paths
  |> List.map ~f:(fun elt ->
    (* only test against the last folder name and not the full path *)
    let file_name = elt |> String.split ~on:'/' |> List.rev |> List.hd_exn in
    elt, Scoring.smith_waterman input_text file_name
  )
  |> List.sort ~compare:(fun a b -> Float.compare (snd b) (snd a))
;;

let draw_ranked_list
      ?(x = 0)
      ?(y = 0)
      ?(font_size = 36)
      ?(font_gap = 0.0)
      ?(draw_score = false)
      ~font
      item_score_tuples
      current_selection
  =
  let font_size = font_size |> float_of_int in

  (* get the max width *)
  let max_width =
    List.fold item_score_tuples ~init:0.0 ~f:(fun acc a ->
      let text_width =
        measure_text_ex font (fst a) font_size font_gap |> Vector2.x
      in
      if Float.( > ) text_width acc then text_width else acc
    )
    |> int_of_float
  in

  let font_height =
    measure_text_ex font "test" font_size font_gap |> Vector2.y |> int_of_float
  in

  let x_offset = x + ((get_screen_width () - max_width) / 2) in
  let y_offset = y in

  (* draw the file names*)
  List.iteri item_score_tuples ~f:(fun i elt ->
    (* draw selected element *)
    if i = current_selection then
      draw_rectangle
        x_offset
        (y + (i * font_height))
        max_width
        font_height
        Color.skyblue
    else
      ();

    (* drow the file name *)
    draw_text_ex
      font
      (fst elt)
      (Vector2.create
         (float_of_int x_offset)
         (float_of_int (y_offset + (i * font_height)))
      )
      font_size
      font_gap
      Color.black
  );

  (* draw the score *)
  if draw_score then
    List.iteri item_score_tuples ~f:(fun i elt ->
      draw_text_ex
        font
        (string_of_float (snd elt))
        (Vector2.create
           (float_of_int (x_offset + max_width + 20))
           (float_of_int (y_offset + (i * font_height)))
        )
        font_size
        font_gap
        Color.black
    )
;;

(* let launch_app path argvs = *)

(** [pad_input ~max_len s] pads [s] with null bytes up to [max_len] before
    passing it to raygui's [text_box]. The raygui binding allocates a C buffer
    exactly the size of the input string, so without padding, typing a new
    character writes one byte past the end of the buffer and corrupts the heap.
    Pre-allocating extra space gives raygui room to append characters safely. *)
let pad_input ?(max_len = 256) s =
  s ^ String.make (max_len - String.length s) '\x00'
;;

type config =
  { font_size : int
  ; code_dir : string
  ; font : Font.t
  ; launchers : Config.launcher list
  }

let initialize_configuration () =
  let config_file = Config.parse_config_file () in
  print_s [%sexp (config_file : Config.config_file)];

  let config_file =
    match
      ( Core_unix.Utsname.sysname (Core_unix.uname ())
      , config_file.mac
      , config_file.linux )
    with
    | "linux", _, Some linux -> linux
    | "mac", Some mac, _ -> mac
    | _ -> config_file.global
  in

  (* set font *)
  let font = load_font config_file.font_dir in
  set_texture_filter (Font.texture font) TextureFilter.Trilinear;

  (* set ray gui font options *)
  Raygui.set_font font;
  Raygui.set_style (Raygui.Control.Default `Text_size) config_file.font_size;

  (* verifiy code_dir exists *)
  if not (SysUtil.file_exists_and_is_dir config_file.code_dir) then
    failwith
      (Printf.sprintf "no such code path directory: %s" config_file.code_dir);

  { font_size = config_file.font_size
  ; code_dir = config_file.code_dir
  ; font
  ; launchers = config_file.launchers
  }
;;

let selection config =
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
      if
        Raylib.is_key_down Raylib.Key.Left_control
        || Raylib.is_key_down Raylib.Key.Right_control
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
      := TextBox.draw
           ~font:config.font
           ~x_pos:x
           ~y_pos:5
           ~width:500
           ~height:70
           true
         |> TextArea.IntArray.to_string;

      (* ranked file list *)
      draw_ranked_list
        !dir_ratio_tuples
        !current_selection
        ~font_size:config.font_size
        ~font:config.font
        ~draw_score:true
        ~x:0
        ~y:100;

      end_drawing ()
    end;

    (* only calculate scores if text changed *)
    if not (phys_equal !new_text !input_text) then (
      input_text := !new_text;
      dir_ratio_tuples := calculate_scores !input_text dirs;
      current_selection := 0
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
    let open Elements in
    rectangle
      ~layout:Vertical
      ~x_position:100
      ~y_position:20
      ~width:0
      ~height:0
      ~color:Color.darkpurple
      [ rectangle
          ~color:Color.purple
          [ text ~color:Color.gold "this is an item in a list" ]
      ; rectangle
          ~color:Color.purple
          [ text ~color:Color.gold "this is an item in the list" ]
      ; rectangle ~color:Color.purple [ text ~color:Color.gold "short text" ]
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
  let config = initialize_configuration () in
  (* selection config *)
  element_list config
;;
