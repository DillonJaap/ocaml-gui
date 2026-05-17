open Raylib
open Core

type direction =
  | Vertical
  | Horizontal

type node =
  | Rectangle of
      { width : int
      ; height : int
      ; x_position : int
      ; y_position : int
      ; color : int
      ; layout : direction
      ; children : node list
      }
  | Text of
      { width : int
      ; height : int
      ; x_position : int
      ; y_position : int
      ; text : string
      ; font_size : int
      ; text_color : Raylib.Color.t
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
      ~width
      ~height
      ~color
      children
  =
  Rectangle { width; height; x_position; y_position; color; layout; children }
;;

let rec calculate_positions node =
  match node with
  | Text text -> Text { text with x_position = 0; y_position = 0 }
  | Rectangle rect when List.length rect.children = 0 -> Rectangle rect
  | Rectangle rect ->
    let (width, height), children =
      List.fold_map rect.children ~init:(0, 0) ~f:(fun acc child ->
        let child = calculate_positions child in
        let acc_width, acc_height = acc in

        match rect.layout with
        | Vertical ->
          ( (max (get_width child) acc_width, acc_height + get_height child)
          , child )
        | Horizontal ->
          ( (acc_width + get_width child, max acc_height (get_height child))
          , child )
      )
    in
    Rectangle { rect with width; height; children }
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
