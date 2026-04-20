open Raylib
open Raygui
open Gui

let calculate_edit_ratios input_text file_paths =
  file_paths
  |> List.map begin fun elt ->
      (* only test against the last folder name and not the full path *)
      let file_name = elt |> String.split_on_char '/' |> List.rev |> List.hd in
      elt, Levenshtein.ratio input_text file_name
    end
;;

let draw_files files x_offset y_offset current_selection =
  let font_size = 28 in

  (* get the max width *)
  let max_width =
    List.fold_left
      begin fun acc a ->
        let text_width = measure_text (fst a) font_size in
        if text_width > acc then text_width else acc
      end
      0
      files
  in

  let x_offset = x_offset + (get_screen_width () / 2) - (max_width / 2) in

  (* draw the file names*)
  List.iteri
    begin fun i elt ->
      (* draw selected element *)
      if i = current_selection then
        draw_rectangle
          x_offset
          (y_offset + 25 + (i * 25))
          max_width
          25
          Color.blue
      else
        ();

      draw_text
        (fst elt)
        x_offset
        (y_offset + 25 + (i * 25))
        font_size
        Color.gold
    end
    files;

  (* draw the Levenshtein ratio *)
  List.iteri
    begin fun i elt ->
      draw_text
        (string_of_float (snd elt))
        (x_offset + max_width + 20)
        (y_offset + 25 + (i * 25))
        font_size
        Color.gold
    end
    files
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
  begin_drawing ();
  clear_background Color.darkgray;

  (* text box *)
  let x = (get_screen_width () / 2) - (500 / 2) |> float_of_int in
  let rect = Rectangle.create x 5.0 500.0 70.0 in
  let new_text, _ = text_box rect (pad_input input_text) true in

  (* file list *)
  draw_files files 0 60 current_selection;

  end_drawing ();
  new_text
;;

let raylib_loop () =
  let input_text = ref "" in
  let current_selection = ref 0 in

  (* Set text size to 24px for ALL controls *)
  set_style (Control.Default `Text_size) 28;
  let files = Os_util.find_git_dirs "/home/dillon/code" in

  let rec loop () =
    if window_should_close () then
      close_window ()
    else begin
      let file_ratio_tuples =
        files
        |> calculate_edit_ratios !input_text
        |> List.sort (fun a b -> Float.compare (snd b) (snd a))
      in

      if is_key_pressed Key.Down then (* TODO limit to list length *)
        current_selection := min (!current_selection + 1) 7
      else if is_key_pressed Key.Up then
        current_selection := max (!current_selection - 1) 0
      else
        ();

      input_text
      := draw_content file_ratio_tuples !input_text !current_selection;
      loop ()
    end
  in
  loop ()
;;

let setup () =
  init_window 1200 700 "raylib [core] example - basic window";
  set_target_fps 60
;;

let () =
  setup ();
  raylib_loop ()
;;
