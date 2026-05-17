open Core
open Raylib

let text = ref [||]
let cursor_position = ref 0

let get_char_pressed_as_int () =
  match get_char_pressed () |> Uchar.to_scalar with
  | 0 -> None
  | c -> Some c
;;

let draw_cursor x_pos y_pos font_size =
  let cursor_height = int_of_float (font_size *. 1.0) in
  draw_rectangle x_pos y_pos 3 cursor_height Color.black
;;

let draw_background ~x_pos ~y_pos ~width ~height = draw_rectangle

let draw ?(font_size = 36.0) ~font ~x_pos ~y_pos ~width ~height focused =
  if !cursor_position > Array.length !text then begin
    cursor_position := Array.length !text
  end;

  (* draw_background *)
  draw_rectangle x_pos y_pos width height Color.skyblue;

  (* draw text and cursor *)
  begin
    begin_scissor_mode x_pos y_pos width height;

    (* vertically center text *)
    let y_pos = y_pos + ((height / 2) - (int_of_float font_size / 2)) in

    (* add margin *)
    let x_pos = x_pos + 5 in

    let base_size = float_of_int (Font.base_size font) in
    let scale = font_size /. base_size in
    let total_advance_x = ref (float_of_int x_pos) in

    (* draw cursor if it is at the begining of the text line *)
    if !cursor_position = 0 && focused then begin
      draw_cursor (int_of_float !total_advance_x) y_pos font_size
    end;

    for i = 0 to Array.length !text - 1 do
      let code_point = !text.(i) in

      let glyph_info = get_glyph_info font code_point in
      let advance_x = float_of_int (GlyphInfo.advance_x glyph_info) *. scale in

      draw_text_codepoint
        font
        !text.(i)
        (Vector2.create !total_advance_x (float_of_int y_pos))
        font_size
        Color.black;

      total_advance_x := !total_advance_x +. advance_x;

      if i = !cursor_position - 1 && focused then begin
        draw_cursor (int_of_float !total_advance_x) y_pos font_size
      end
    done;

    end_scissor_mode ()
  end;

  let rec handle_keys () =
    match get_key_pressed () with
    | Key.Backspace ->
      if !cursor_position >= 1 then begin
        let pos = !cursor_position - 1 in
        TextArea.IntArray.delete text pos pos;
        cursor_position := !cursor_position - 1
      end;
      handle_keys ()
    | Key.Delete ->
      if !cursor_position < Array.length !text then begin
        let pos = !cursor_position in
        TextArea.IntArray.delete text pos pos
      end;
      handle_keys ()
    | Key.Left ->
      cursor_position := max 0 (!cursor_position - 1);
      handle_keys ()
    | Key.Right ->
      cursor_position := min (Array.length !text) (!cursor_position + 1);
      handle_keys ()
    | Key.Null | _ -> ()
  in

  let rec handle_chars () =
    match get_char_pressed_as_int () with
    | Some c ->
      TextArea.IntArray.insert text [| c |] !cursor_position;
      cursor_position := !cursor_position + 1;
      handle_chars ()
    | None -> ()
  in

  if focused then begin
    handle_keys ();
    handle_chars ()
  end;
  !text
;;
