open Gui
open Core
open Raylib

(* TODO package this with the program or use system default? *)

let calculate_edit_ratios input_text file_paths =
  file_paths
  |> List.map ~f:begin fun elt ->
      (* only test against the last folder name and not the full path *)
      let file_name = elt |> String.split ~on:'/' |> List.rev |> List.hd_exn in
      elt, Levenshtein.ratio input_text file_name
    end
;;

let draw_files
      ?(x = 0)
      ?(y = 0)
      ?(font_size = 36.0)
      ?(font_gap = 0.5)
      files
      current_selection
      font
  =
  (* get the max width *)
  let max_width =
    List.fold files ~init:0.0 ~f:begin fun acc a ->
        let text_width =
          measure_text_ex font (fst a) font_size font_gap |> Vector2.x in
        if Float.( > ) text_width acc then text_width else acc
      end
    |> int_of_float in

  let font_height =
    measure_text_ex font "test" font_size font_gap |> Vector2.y |> int_of_float
  in

  let x_offset = x + ((get_screen_width () - max_width) / 2) in
  let y_offset = y in

  (* draw the file names*)
  List.iteri files ~f:begin fun i elt ->
      (* draw selected element *)
      if i = current_selection then
        draw_rectangle x_offset (y + (i * font_height)) max_width 36 Color.blue
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

  (* draw the Levenshtein ratio *)
    List.iteri files ~f:begin fun i elt ->
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

(** [pad_input ~max_len s] pads [s] with null bytes up to [max_len] before
    passing it to raygui's [text_box]. The raygui binding allocates a C buffer
    exactly the size of the input string, so without padding, typing a new
    character writes one byte past the end of the buffer and corrupts the heap.
    Pre-allocating extra space gives raygui room to append characters safely. *)
let pad_input ?(max_len = 256) s =
  s ^ String.make (max_len - String.length s) '\x00'
;;

let draw_content files selected font ~input =
  begin_drawing ();
  clear_background Color.darkgray;

  (* text box *)
  let x = (get_screen_width () - 500) / 2 |> float_of_int in
  let rect = Rectangle.create x 5.0 500.0 70.0 in
  let new_text, _ = Raygui.text_box rect (pad_input input) true in

  (* file list *)
  draw_files files selected font ~x:0 ~y:100;

  end_drawing ();
  new_text
;;

let raylib_loop () =
  (* set default font *)
  let font =
    load_font "/home/dillon/code/ocaml-gui/assets/SpaceMono-Regular.ttf" in
  set_texture_filter (Font.texture font) TextureFilter.Trilinear;

  Raygui.set_font font;
  Raygui.set_style (Raygui.Control.Default `Text_size) 36;

  (* calculate scores function  *)
  let calculate_scores dirs input_text =
    dirs
    |> calculate_edit_ratios input_text
    |> List.sort ~compare:(fun a b -> Float.compare (snd b) (snd a)) in

  (* values that get mutated by user input *)
  let input_text = ref "" in
  let current_selection = ref 0 in

  (* directories *)
  let mac_home = "/Users/DJaap" in
  let linux_home = "/home/dillon" in
  let home_dir =
    if Stdlib.Sys.file_exists linux_home && Stdlib.Sys.is_directory linux_home
    then
      linux_home
    else if Stdlib.Sys.file_exists mac_home && Stdlib.Sys.is_directory mac_home
    then
      mac_home
    else
      failwith
      @@ Printf.sprintf
           "no such directories, \"%s\" or \"%s\""
           linux_home
           mac_home in
  let code_dir = home_dir ^ "/code" in

  (* execution path *)
  let kitty_exec_path =
    if Stdlib.Sys.file_exists linux_home then
      "/usr/bin/kitty"
    else if Stdlib.Sys.file_exists mac_home then
      "/Applications/kitty.app/Contents/MacOS/kitty"
    else
      failwith
      @@ Printf.sprintf "no such execs, \"%s\" or \"%s\"" linux_home mac_home
  in

  (* code project directior info *)
  let dirs = Os_util.find_git_dirs code_dir in
  let num_files = List.length dirs in
  let dir_ratio_tuples = ref (calculate_scores dirs !input_text) in

  let rec loop () =
    (* close window and exit loop *)
    if not (window_should_close ()) then begin
      (* handle keypresses *)
        let open Key in
        begin match get_key_pressed () with
        | Down ->
          current_selection := min (!current_selection + 1) (num_files - 1)
        | Up -> current_selection := max (!current_selection - 1) 0
        (* open kitty window in selected directory *)
        | Enter ->
          (* Stdlib.Sys.set_signal Stdlib.Sys.sigchld Stdlib.Sys.Signal_ignore; *)
          let dir = List.nth_exn !dir_ratio_tuples !current_selection |> fst in
          Os_util.daemonize
            ~prog:kitty_exec_path
            ~argv:[| "kitty"; "--directory"; dir |];
          close_window ();
          exit 0
        (* open kitty tab in selected directory *)
        | Tab ->
          let _ =
            let dir = List.nth_exn !dir_ratio_tuples !current_selection |> fst in
            let file_name =
              dir |> String.split ~on:'/' |> List.rev |> List.hd_exn in

            Stdlib.Sys.command
            @@ Printf.sprintf
                 "kitten @ launch --type tab --title  \"%s\" --cwd \"%s\""
                 file_name
                 dir in
          close_window ();
          exit 0
        | _ -> ()
        end;

        (* get new input from text box *)
        let new_text =
          draw_content
            !dir_ratio_tuples
            !current_selection
            font
            ~input:!input_text in

        (* only calculate scores if text changed *)
        if not (phys_equal new_text !input_text) then (
          input_text := new_text;
          dir_ratio_tuples := calculate_scores dirs !input_text
        ) else
          ();
        loop ()
    end (* raylib window should close *) else begin
          close_window ();
          exit 0
        end in
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
