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
  (* Set text size to 24px for ALL controls *)
  Raygui.set_style (Raygui.Control.Default `Text_size) 28;

  let calculate_scores dirs input_text =
    dirs
    |> calculate_edit_ratios input_text
    |> List.sort ~compare:(fun a b -> Float.compare (snd b) (snd a))
  in

  let input_text = ref "" in
  let current_selection = ref 0 in

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
           mac_home
  in
  let code_dir = home_dir ^ "/code" in

  let kitty_exec_path =
    if Stdlib.Sys.file_exists linux_home then
      "/usr/bin/kitty"
    else if Stdlib.Sys.file_exists mac_home then
      "/Applications/kitty.app/Contents/MacOS/kitty"
    else
      failwith
      @@ Printf.sprintf "no such execs, \"%s\" or \"%s\"" linux_home mac_home
  in

  let dirs = Os_util.find_git_dirs code_dir in
  let num_files = List.length dirs in
  let dir_ratio_tuples = ref (calculate_scores dirs !input_text) in

  let rec loop () =
    (* close window and exit loop *)
    if not (Raylib.window_should_close ()) then begin (* handle keypresses *)
      let open Raylib.Key in
      begin match Raylib.get_key_pressed () with
      | Down -> current_selection := min (!current_selection + 1) (num_files - 1)
      | Up -> current_selection := max (!current_selection - 1) 0
      (* open kitty window in selected directory *)
      | Enter ->
        (* Stdlib.Sys.set_signal Stdlib.Sys.sigchld Stdlib.Sys.Signal_ignore; *)
        let dir = List.nth_exn !dir_ratio_tuples !current_selection |> fst in
        Os_util.daemonize
          ~prog:kitty_exec_path
          ~argv:[| "kitty"; "--directory"; dir |];
        Raylib.close_window ();
        exit 0
      (* open kitty tab in selected directory *)
      | Tab ->
        let _ =
          let dir = List.nth_exn !dir_ratio_tuples !current_selection |> fst in
          let file_name =
            dir |> String.split ~on:'/' |> List.rev |> List.hd_exn
          in
          Stdlib.Sys.command
          @@ Printf.sprintf
               "kitten @ launch --type tab --title  \"%s\" --cwd \"%s\""
               file_name
               dir
        in
        Raylib.close_window ();
        exit 0
      | _ -> ()
      end;

      (* get new input from text box *)
      let new_text =
        draw_content !dir_ratio_tuples !input_text !current_selection
      in

      (* only calculate scores if text changed *)
      if not (phys_equal new_text !input_text) then (
        input_text := new_text;
        dir_ratio_tuples := calculate_scores dirs !input_text)
      else
        ();
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
