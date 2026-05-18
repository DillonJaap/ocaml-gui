open Raylib
open Core

type direction =
  | Vertical
  | Horizontal

type text =
  { content : string
  ; font_size : int
  ; text_color : Raylib.Color.t
  }

type node =
  { width : int
  ; height : int
  ; x_position : int
  ; y_position : int
  ; color : Raylib.Color.t
  ; layout : direction
  ; children : node list
  ; text : text option
  }

let get_width node =
  match node with
  | Rectangle r -> r.width
  | Text t -> t.width
;;

let get_height node =
  match node with
  | Rectangle r -> r.height
  | Text t -> t.height
;;

let get_x node =
  match node with
  | Rectangle r -> r.x_position
  | Text t -> t.y_position
;;

let get_y node =
  match node with
  | Rectangle r -> r.x_position
  | Text t -> t.y_position
;;

let set_pos node x y =
  match node with
  | Rectangle r -> Rectangle { r with x_position = x; y_position = y }
  | Text t -> Text { t with x_position = x; y_position = y }
;;

let text ?(font_size = 36) ~color:text_color text =
  Text
    { width = measure_text text font_size
    ; height = font_size
    ; x_position = 0
    ; y_position = 0
    ; text
    ; font_size
    ; text_color
    }
;;

let rectangle
      ?(layout = Horizontal)
      ?(x_position = 0)
      ?(y_position = 0)
      ?(width = 0)
      ?(height = 0)
      ?(color = Color.blank)
      children
  =
  Rectangle { width; height; x_position; y_position; color; layout; children }
;;

let rec calculate_sizes node =
  match node with
  | Text text -> Text text
  | Rectangle rect when List.length rect.children = 0 -> Rectangle rect
  | Rectangle rect ->
    let (~width, ~height), children =
      List.fold_map
        rect.children
        ~init:(~width:0, ~height:0)
        ~f:(fun (~width, ~height) child ->
          let child = calculate_sizes child in
          match rect.layout with
          | Vertical ->
            let height = height + get_height child in
            let width = max (get_width child) width in
            (~width, ~height), child
          | Horizontal ->
            let height = max height (get_height child) in
            let width = width + get_width child in
            (~width, ~height), child
      )
    in
    Rectangle { rect with width; height; children }
;;

let rec calculate_positions node =
  match node with
  | Text text -> Text text
  | Rectangle rect when List.length rect.children = 0 -> Rectangle rect
  | Rectangle rect ->
    let children =
      List.folding_map
        rect.children
        ~init:(~x:rect.x_position, ~y:rect.y_position)
        ~f:(fun (~x, ~y) child ->
          (* let child = calculate_positions child in *)
          match rect.layout with
          | Vertical ->
            let child = set_pos child x y in
            let y = y + get_height child in
            (~x, ~y), calculate_positions child
          | Horizontal ->
            let child = set_pos child x y in
            let x = x + get_width child in
            (~x, ~y), calculate_positions child
      )
    in
    Rectangle { rect with children }
;;

let rec render node =
  match node with
  | Rectangle rect ->
    draw_rectangle
      rect.x_position
      rect.y_position
      rect.width
      rect.height
      rect.color;
    List.iter rect.children ~f:(fun child -> render child)
  | Text text ->
    draw_text
      text.text
      text.x_position
      text.y_position
      text.font_size
      text.text_color
;;

let list ~direction ~position ~color children ~font_size =
  let x, y = position in

  let width, height =
    List.fold children ~init:(0, 0) ~f:(fun acc el ->
      let el_width, el_height = el in
      let acc_width, acc_height = acc in

      match direction with
      | Vertical -> max el_width acc_width, acc_height + el_height
      | Horizontal -> acc_width + el_width, max acc_height el_height
    )
  in

  draw_rectangle x y width height color
;;
