open Raylib
open Core

type direction =
  | Vertical
  | Horizontal

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
  ; align : align
  ; kind : node_kind
  }

let text
      ?(font_size = 36)
      ?(padding = { left = 0; right = 0; top = 0; bottom = 0 })
      ?padding_all
      ?(align = Left)
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
  ; align
  ; kind = Text { content = text; font_size; text_color; font }
  }
;;

let rectangle
      ?(layout = Horizontal)
      ?(x_position = 0)
      ?(y_position = 0)
      ?(align = Left)
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
  ; align
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

let rec calculate_sizes node =
  let calculate_fill_size children layout =
    let aligned_axis_sizing ~align ~across =
      let align_total_size = align.total + align.size + align.pad_before + align.pad_after in
      let across_total_size =
        max across.total (across.size + across.pad_before + across.pad_after)
      in
      align_total_size, across_total_size
    in

    let ~width, ~height =
      List.fold children ~init:(~width:0, ~height:0) ~f:(fun (~width, ~height) child ->
        let v_axis =
          { total = height
          ; size = child.height
          ; pad_before = child.padding.top
          ; pad_after = child.padding.bottom
          }
        in
        let h_axis =
          { total = width
          ; size = child.width
          ; pad_before = child.padding.left
          ; pad_after = child.padding.right
          }
        in

        match layout with
        | Vertical ->
          let height, width = aligned_axis_sizing ~align:v_axis ~across:h_axis in
          ~width, ~height
        | Horizontal ->
          let width, height = aligned_axis_sizing ~align:h_axis ~across:v_axis in
          ~width, ~height
      )
    in
    width, height
  in

  match node.kind with
  | Text text ->
    let size = measure_text_ex text.font text.content (float_of_int text.font_size) 0.0 in
    { node with width = Vector2.x size |> int_of_float; height = Vector2.y size |> int_of_float }
  | Rectangle rect ->
    let children = List.map rect.children ~f:(fun child -> calculate_sizes child) in

    let width, height =
      match node.width_sizing, node.height_sizing with
      | Fixed w, Fixed h -> w, h
      | Fill, Fill -> calculate_fill_size children rect.layout
      | Fixed w, Fill ->
        let _, h = calculate_fill_size children rect.layout in
        w, h
      | Fill, Fixed h ->
        let w, _ = calculate_fill_size children rect.layout in
        w, h
    in
    { node with width; height; kind = Rectangle { rect with children } }
;;

let rec calculate_positions node =
  (* apply padding, assuming align left and justify top *)
  let node =
    { node with
      x_position = node.x_position + node.padding.left
    ; y_position = node.y_position + node.padding.top
    }
  in

  match node.kind with
  | Text text -> node
  | Rectangle rect when List.length rect.children = 0 -> node
  | Rectangle rect ->
    let children =
      List.folding_map
        rect.children
        ~init:(~x:node.x_position, ~y:node.y_position)
        ~f:(fun (~x, ~y) child ->
          (* alignment / justify *)
          let child_x =
            match child.align with
            | Left -> x
            | Right -> x + (node.width - child.width)
          in

          (* layout *)
          match rect.layout with
          | Vertical ->
            let child = { child with x_position = child_x; y_position = y } in
            let y = y + child.height + child.padding.top + child.padding.bottom in

            (~x, ~y), calculate_positions child
          | Horizontal ->
            let child = { child with x_position = child_x; y_position = y } in
            let x = x + child.width + child.padding.left + child.padding.right in
            (~x, ~y), calculate_positions child
      )
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
