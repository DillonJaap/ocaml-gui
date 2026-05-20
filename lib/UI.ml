open Raylib
open Core

type direction =
  | Vertical
  | Horizontal

type axis =
  | XAxis
  | YAxis

type sizing =
  | Fixed of int
  | Fill

type align =
  | Left
  | Right

type padding =
  { left : int
  ; right : int
  ; top : int
  ; bottom : int
  }

type node_kind =
  | Rectangle of
      { color : Raylib.Color.t
      ; layout : direction
      ; children : node list
      }
  | Text of
      { content : string
      ; font_size : int
      ; text_color : Raylib.Color.t
      ; font : Font.t
      }

and node =
  { width_sizing : sizing
  ; height_sizing : sizing
  ; width : int
  ; height : int
  ; x_position : int
  ; y_position : int
  ; padding : padding
  ; kind : node_kind
  }

let text
      ?(font_size = 36)
      ?(padding = { left = 0; right = 0; top = 0; bottom = 0 })
      ?padding_all
      ?(text_color = Color.black)
      ?(font = get_font_default ())
      text
  =
  let padding =
    match padding_all with
    | Some p -> { left = p; right = p; top = p; bottom = p }
    | none -> padding
  in
  { width_sizing = Fill
  ; height_sizing = Fill
  ; width = 0
  ; height = 0
  ; padding
  ; x_position = 0
  ; y_position = 0
  ; kind = Text { content = text; font_size; text_color; font }
  }
;;

let rectangle
      ?(layout = Horizontal)
      ?(x_position = 0)
      ?(y_position = 0)
      ?(width = Fill)
      ?(height = Fill)
      ?(padding = { left = 0; right = 0; top = 0; bottom = 0 })
      ?padding_all
      ?(color = Color.blank)
      children
  =
  let padding =
    match padding_all with
    | Some p -> { left = p; right = p; top = p; bottom = p }
    | none -> padding
  in

  { width_sizing = width
  ; height_sizing = height
  ; width = 0
  ; height = 0
  ; padding
  ; x_position
  ; y_position
  ; kind = Rectangle { color; layout; children }
  }
;;

let empty () = rectangle []

type axis_sizing =
  { total : int
  ; size : int
  ; pad_before : int
  ; pad_after : int
  }

let node_extent child axis =
  match axis with
  | XAxis -> child.width + child.padding.left + child.padding.right
  | YAxis -> child.height + child.padding.top + child.padding.bottom
;;

let rec calculate_sizes node =
  let sum_axis children axis = List.sum (module Int) children ~f:(fun c -> node_extent c axis) in

  let max_axis children axis =
    List.fold children ~init:0 ~f:(fun acc c -> max acc (node_extent c axis))
  in

  match node.kind with
  | Text text ->
    let size = measure_text_ex text.font text.content (float_of_int text.font_size) 0.0 in
    { node with width = Vector2.x size |> int_of_float; height = Vector2.y size |> int_of_float }
  | Rectangle rect ->
    let children = List.map rect.children ~f:(fun child -> calculate_sizes child) in

    let width =
      match node.width_sizing with
      | Fixed w -> w
      | Fill ->
        ( match rect.layout with
          | Vertical -> max_axis children XAxis
          | Horizontal -> sum_axis children XAxis
        )
    in

    let height =
      match node.height_sizing with
      | Fixed w -> w
      | Fill ->
        ( match rect.layout with
          | Vertical -> sum_axis children YAxis
          | Horizontal -> max_axis children YAxis
        )
    in

    { node with width; height; kind = Rectangle { rect with children } }
;;

let rec calculate_positions node =
  let node =
    { node with
      x_position = node.x_position + node.padding.left
    ; y_position = node.y_position + node.padding.top
    }
  in

  match node.kind with
  | Text text -> node
  | Rectangle rect ->
    let children =
      List.folding_map
        rect.children
        ~init:(~x:node.x_position, ~y:node.y_position)
        ~f:(fun (~x, ~y) child ->
          (* layout *)
          match rect.layout with
          | Vertical ->
            let child = { child with x_position = x; y_position = y } in
            let y = y + node_extent child YAxis in
            (~x, ~y), child
          | Horizontal ->
            let child = { child with x_position = x; y_position = y } in
            let x = x + node_extent child XAxis in
            (~x, ~y), child
      )
      |> List.map ~f:(fun child -> calculate_positions child)
    in

    { node with kind = Rectangle { rect with children } }
;;

let rec render node =
  match node.kind with
  | Rectangle rect ->
    draw_rectangle node.x_position node.y_position node.width node.height rect.color;
    List.iter rect.children ~f:(fun child -> render child)
  | Text text ->
    draw_text_ex
      text.font
      text.content
      (Vector2.create (float_of_int node.x_position) (float_of_int node.y_position))
      (float_of_int text.font_size)
      0.0
      text.text_color
;;
