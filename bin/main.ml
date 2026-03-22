open Raylib
open Gui

let readdir_option dir_handle =
  match Unix.readdir dir_handle with
  | str -> Some str
  | exception End_of_file -> None
;;

(* util *)
let ls dir =
  let dir_handle = Unix.opendir dir in
  let _ = Printf.sprintf "hello you are %s" "dillon" in

  let rec loop files =
    match readdir_option dir_handle with
    (* ignore relative directories *)
    | Some file when file = "." || file = ".." -> loop files
    | Some file -> loop (file :: files)
    | None -> files
  in
  loop []
;;

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
  clear_background Color.darkgray;
  draw_files files
;;

let raylib_loop files =
  let rec loop () =
    if window_should_close () then
      close_window ()
    else
      let open Raylib in
      begin_drawing ();
      draw_content files;
      end_drawing ();
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
