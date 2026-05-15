open Core
open Raylib

let text = ref [||]
let cursor_position = ref 0

let get_char_pressed_as_int () =
  match get_char_pressed () |> Uchar.to_scalar with
  | 0 -> None
  | c -> Some c
;;

let draw_cursor text =
  if !cursor_position = 0 then
    draw_rectangle 0 20 4 36 Color.darkgreen
;;

let draw ?(font_size = 36.0) ~font is_focused =
  (* let c_text = TextArea.IntArray.to_carray !text in *)
  (* draw_text_codepoints *)
  (*   font *)
  (*   c_text *)
  (*   (Vector2.create 0.0 20.0) *)
  (*   font_size *)
  (*   0.0 *)
  (*   Color.purple; *)
  draw_cursor !text;

  let total_advance_x = ref 0 in
  for i = 0 to Array.length !text - 1 do
    let code_point = !text.(i) in

    let glyph_info = get_glyph_info font code_point in
    let offset_x = GlyphInfo.offset_x glyph_info in
    let offset_y = GlyphInfo.offset_y glyph_info in
    let advance_x = GlyphInfo.advance_x glyph_info in

    draw_text_codepoint
      font
      !text.(i)
      (Vector2.create
         (float_of_int (!total_advance_x + offset_x))
         (float_of_int (offset_y + 20))
      )
      font_size
      Color.purple;

    total_advance_x := !total_advance_x + advance_x
  done;

  let rec handle_input () =
    match get_key_pressed (), get_char_pressed_as_int () with
    | Key.Backspace, _ ->
      let pos = Array.length !text - 1 in
      TextArea.IntArray.delete text pos pos;
      cursor_position := max (!cursor_position - 1) 0;
      handle_input ()
    | _, Some c ->
      TextArea.IntArray.insert text [| c |] (Array.length !text - 1);
      cursor_position := !cursor_position + 1;
      handle_input ()
    | _, _ -> ()
  in

  if is_focused then (
    match
      get_key_pressed ()
    with
    | _ -> handle_input ()
  );

  !text
;;
