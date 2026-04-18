open Raylib
open Raygui
open Gui
open Os_util

let calculate_edit_ratios input_text files =
  List.map (fun elt -> elt, Levenshtein.ratio input_text elt) files
;;

(* draw stuff *)
let draw_files files =
  let font_size = 20 in
  let x_offset = (get_screen_width () / 2) - 150 in

  (* draw the file names*)
  List.iteri
    begin fun i elt ->
      draw_text (fst elt) x_offset (25 + (i * 25)) font_size Color.gold
    end
    files;

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

  (* draw the Levenshtein ratio *)
  List.iteri
    begin fun i elt ->
      draw_text
        (string_of_float (snd elt))
        (x_offset + max_width + 20)
        (25 + (i * 25))
        font_size
        Color.gold
    end
    files
;;

let draw_content files =
  begin_drawing ();
  clear_background Color.darkgray;

  (* text box *)
  let rect = Rectangle.create 5.0 5.0 50.0 500.0 in
  let _ = text_box rect "this is a test" true in

  (* file list *)
  draw_files files;

  end_drawing ()
;;

let raylib_loop files =
  let rec loop () =
    if window_should_close () then
      close_window ()
    else
      draw_content files;
    loop ()
  in
  loop ()
;;

let setup () =
  print_string "hello";
  init_window 800 450 "raylib [core] example - basic window";
  set_target_fps 60;

  ls "."
  |> calculate_edit_ratios "opam"
  |> List.sort (fun a b -> Float.compare (snd b) (snd a))
;;

let () = setup () |> raylib_loop
