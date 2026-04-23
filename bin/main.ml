open Gui
open Core

let calculate_edit_ratios input_text file_paths =
  file_paths
  |> List.map ~f:begin fun elt ->
      (* only test against the last folder name and not the full path *)
      let file_name = elt |> String.split ~on:'/' |> List.rev |> List.hd_exn in
      elt, Levenshtein.ratio input_text file_name
    end
;;

let draw_files files x_offset y_offset current_selection =
  let font_size = 18 in

  (* get the max width *)
  let max_width =
    List.fold files ~init:0 ~f:begin fun acc a ->
        let text_width = Raylib.measure_text (fst a) font_size in
        if text_width > acc then text_width else acc
      end
  in

  let x_offset = x_offset + ((Raylib.get_screen_width () - max_width) / 2) in

  (* draw the file names*)
  List.iteri files ~f:begin fun i elt ->
      (* draw selected element *)
      if i = current_selection then
        Raylib.draw_rectangle
          x_offset
          (y_offset + 25 + (i * 25))
          max_width
          25
          Raylib.Color.blue
      else
        ();

      (* drow the file name *)
      Raylib.draw_text
        (fst elt)
        x_offset
        (y_offset + 25 + (i * 25))
        font_size
        Raylib.Color.gold
    end;

  (* draw the Levenshtein ratio *)
    List.iteri files ~f:begin fun i elt ->
      Raylib.draw_text
        (string_of_float (snd elt))
        (x_offset + max_width + 20)
        (y_offset + 25 + (i * 25))
        font_size
        Raylib.Color.gold
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

let draw_content files input_text current_selection =
  Raylib.begin_drawing ();
  Raylib.clear_background Raylib.Color.darkgray;

  (* text box *)
  let x = (Raylib.get_screen_width () - 500) / 2 |> float_of_int in
  let rect = Raylib.Rectangle.create x 5.0 500.0 70.0 in
  let new_text, _ = Raygui.text_box rect (pad_input input_text) true in

  (* file list *)
  draw_files files 0 60 current_selection;

  Raylib.end_drawing ();
  new_text
;;

let raylib_loop () =
  let input_text = ref "" in
  let current_selection = ref 0 in

  let mac_dir = "/Users/DJaap/code" in
  let linux_dir = "/home/dillon/code" in

  let code_dir =
    if Stdlib.Sys.file_exists linux_dir && Stdlib.Sys.is_directory linux_dir
    then
      "home/dillon/code"
    else if Stdlib.Sys.file_exists mac_dir && Stdlib.Sys.is_directory mac_dir
    then
      "/Users/DJaap/code"
    else
      failwith
      @@ Printf.sprintf
           "no such directories, \"%s\" or \"%s\""
           linux_dir
           mac_dir
  in

  let kitty_exec_path =
    if Stdlib.Sys.file_exists linux_dir then
      "/usr/bin/kitty"
    else if Stdlib.Sys.file_exists mac_dir then
      "/Applications/kitty.app/Contents/MacOS/kitty"
    else
      failwith
      @@ Printf.sprintf "no such execs, \"%s\" or \"%s\"" linux_dir mac_dir
  in

  let dirs = Os_util.find_git_dirs code_dir in
  let num_files = List.length dirs in

  let rec loop () =
    (* close window and exit loop *)
    if not (Raylib.window_should_close ()) then begin
      let dir_ratio_tuples =
        dirs
        |> calculate_edit_ratios !input_text
        |> List.sort ~compare:(fun a b -> Float.compare (snd b) (snd a))
      in

      (* handle keypresses *)
      if Raylib.is_key_pressed Raylib.Key.Down then
        current_selection := min (!current_selection + 1) (num_files - 1)
      else if Raylib.is_key_pressed Raylib.Key.Up then
        current_selection := max (!current_selection - 1) 0
      else if Raylib.is_key_pressed Raylib.Key.Enter then begin
        Os_util.daemonize
          ~prog:kitty_exec_path
          ~argv:
            [| "kitty"
             ; "--directory"
             ; List.nth_exn dir_ratio_tuples !current_selection |> fst
            |];
        Raylib.close_window ();
        exit 0
      end
      else
        ();

      input_text := draw_content dir_ratio_tuples !input_text !current_selection;
      loop ()
    end
    (* raylib window should close *)
    else begin
      Raylib.close_window ();
      exit 0
    end
  in
  loop ()
;;

let setup () =
  let window_width = 1200 in
  let window_height = 800 in

  Raylib.init_window window_width window_height "Project Launcher";

  (* theoretically center the window *)
  Raylib.set_window_position
    ((Raylib.get_screen_width () - window_width) / 2)
    ((Raylib.get_screen_height () - window_height) / 2);

  (* target FPS *)
  Raylib.set_target_fps 60;

  (* Set text size to 24px for ALL controls *)
  Raygui.set_style (Raygui.Control.Default `Text_size) 18;

  (* remove window decoration *)
  Raylib.set_window_state [ Raylib.ConfigFlags.Window_undecorated ]
;;

let () =
  setup ();
  raylib_loop ()
;;
