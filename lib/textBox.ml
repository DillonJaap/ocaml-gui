open Core
open Raylib

let get_input () =
  match get_char_pressed () |> Uchar.to_scalar with
  | 0 -> None
  | c -> Some c
;;

let draw ?(font_size = 36.0) ~font text is_focused =
  let c_text = TextArea.IntArray.to_carray text in
  draw_text_codepoints
    font
    c_text
    (Vector2.create 0.0 20.0)
    font_size
    0.0
    Color.purple;

  if is_focused then (
    match
      get_key_pressed ()
    with
    | Key.Backspace ->
      let text = ref text in
      let pos = Array.length !text - 1 in
      TextArea.IntArray.delete text pos pos;
      !text
    | _ ->
      begin match get_input () with
      | None -> text
      | Some c ->
        let text = ref text in
        TextArea.IntArray.insert text [| c |] (Array.length !text - 1);
        !text
      end
  ) else
    text
;;
