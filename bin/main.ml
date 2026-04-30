open Gui
open Core
open Raylib

(* TODO package this with the program or use system default? *)

let calculate_scores input_text file_paths =
  file_paths
  |> List.map ~f:begin fun elt ->
      (* only test against the last folder name and not the full path *)
      let file_name = elt |> String.split ~on:'/' |> List.rev |> List.hd_exn in
      elt, Scoring.smith_waterman input_text file_name
    end
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
    List.fold item_score_tuples ~init:0.0 ~f:begin fun acc a ->
        let text_width =
          measure_text_ex font (fst a) font_size font_gap |> Vector2.x
        in
        if Float.( > ) text_width acc then text_width else acc
      end
    |> int_of_float
  in

  let font_height =
    measure_text_ex font "test" font_size font_gap |> Vector2.y |> int_of_float
  in

  let x_offset = x + ((get_screen_width () - max_width) / 2) in
  let y_offset = y in

  (* draw the file names*)
  List.iteri item_score_tuples ~f:begin fun i elt ->
      (* draw selected element *)
      if i = current_selection then
        draw_rectangle
          x_offset
          (y + (i * font_height))
          max_width
          font_height
          Color.blue
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
        Color.gold
    end;

  (* draw the score *)
  if draw_score then
    List.iteri item_score_tuples ~f:begin fun i elt ->
        draw_text_ex
          font
          (string_of_float (snd elt))
          (Vector2.create
             (float_of_int (x_offset + max_width + 20))
             (float_of_int (y_offset + (i * font_height)))
          )
          font_size
          font_gap
          Color.gold
      end
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

let raylib_loop () =
  let config = initialize_configuration () in
  let neovide_exec_path = (config.launchers |> List.hd_exn).path in

  (* values that get mutated by user input *)
  let input_text = ref "" in
  let current_selection = ref 0 in
  let _launcher = ref None in

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

    let open Key in
    (* handle keypresses *)
    begin
      begin match get_key_pressed () with
      | Down -> current_selection := min (!current_selection + 1) (num_files - 1)
      | Up -> current_selection := max (!current_selection - 1) 0
      | Enter ->
        (* open kitty window in selected directory *)
        let dir = List.nth_exn !dir_ratio_tuples !current_selection |> fst in
        (* SysUtil.daemonize *)
        (*   ~prog:kitty_exec_path *)
        (*   ~argv:[| "kitty"; "--directory"; dir |]; *)
        SysUtil.daemonize
          ~prog:neovide_exec_path
          ~argv:[| "neovide"; "--chdir"; dir |];

        close_window ();
        exit 0
      | Tab ->
        (* open kitty tab in selected directory *)
        let dir = List.nth_exn !dir_ratio_tuples !current_selection |> fst in
        let file_name =
          dir |> String.split ~on:'/' |> List.rev |> List.hd_exn
        in

        Stdlib.Sys.command
          (Printf.sprintf
             "kitten @ launch --type tab --title  \"%s\" --cwd \"%s\""
             file_name
             dir
          )
        |> ignore;

        close_window ();
        exit 0
      | _ -> ()
      end
    end;

    (* get new input from text box *)
    let new_text =
      begin
        begin_drawing ();
        clear_background Color.darkgray;

        (* text box *)
        let x = (get_screen_width () - 500) / 2 |> float_of_int in
        let rect = Rectangle.create x 5.0 500.0 70.0 in
        let new_text, _ = Raygui.text_box rect (pad_input !input_text) true in

        (* ranked file list *)
        draw_ranked_list
          !dir_ratio_tuples
          !current_selection
          ~font_size:config.font_size
          ~font:config.font
          ~draw_score:true
          ~x:0
          ~y:100;

        end_drawing ();
        new_text
      end
    in

    (* only calculate scores if text changed *)
    if not (phys_equal new_text !input_text) then (
      input_text := new_text;
      dir_ratio_tuples := calculate_scores !input_text dirs
    );

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
  raylib_loop ()
;;
