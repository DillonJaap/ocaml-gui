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
  | Grow

type align =
  | Left
  | Right

type padding =
  { left : int
  ; right : int
  ; top : int
  ; bottom : int
  }

type container_type =
  { color : Raylib.Color.t
  ; children : node list
  }

and node_kind =
  | Container of container_type
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
  ; layout : direction
  ; padding : padding
  ; kind : node_kind
  }

type axis_sizing =
  { total : int
  ; size : int
  ; pad_before : int
  ; pad_after : int
  }

let node_outer_span node axis =
  match axis with
  | XAxis -> node.width + node.padding.left + node.padding.right
  | YAxis -> node.height + node.padding.top + node.padding.bottom
;;

let node_inner_span node axis =
  match axis with
  | XAxis -> node.width
  | YAxis -> node.height
;;

let sizing_for_axis node axis =
  match axis with
  | XAxis -> node.width_sizing
  | YAxis -> node.height_sizing
;;

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
  ; layout = Horizontal
  ; kind = Text { content = text; font_size; text_color; font }
  }
;;

let container
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
  ; layout
  ; kind = Container { color; children }
  }
;;

let empty () = container []

let rec fit_sizing node =
  let sum_axis children axis =
    List.sum (module Int) children ~f:(fun c -> node_outer_span c axis)
  in

  let max_axis children axis =
    List.fold children ~init:0 ~f:(fun acc c -> max acc (node_outer_span c axis))
  in

  match node.kind with
  | Text text ->
    let size = measure_text_ex text.font text.content (float_of_int text.font_size) 0.0 in
    { node with width = Vector2.x size |> int_of_float; height = Vector2.y size |> int_of_float }
  | Container rect ->
    let children = List.map rect.children ~f:(fun child -> fit_sizing child) in

    let width =
      match node.width_sizing with
      | Fixed w -> w
      | Fill | Grow ->
        ( match node.layout with
          | Vertical -> max_axis children XAxis
          | Horizontal -> sum_axis children XAxis
        )
    in

    let height =
      match node.height_sizing with
      | Fixed w -> w
      | Fill | Grow ->
        ( match node.layout with
          | Vertical -> sum_axis children YAxis
          | Horizontal -> max_axis children YAxis
        )
    in

    { node with width; height; kind = Container { rect with children } }
;;

let rec grow_sizing node : node =
  let grow_main_axis axis node =
    match node.kind with
    | Text _ -> node
    | Container rect ->
      let children_outer_span =
        List.fold rect.children ~init:0 ~f:(fun acc child -> acc + node_outer_span child axis)
      in

      let remaining_space = node_inner_span node axis - children_outer_span in

      let grow_child_count =
        List.fold rect.children ~init:0 ~f:(fun acc child ->
          match sizing_for_axis child axis with
          | Grow -> acc + 1
          | _ -> acc
        )
      in

      let extra_per_grow_child, remainder =
        if grow_child_count = 0 then
          0, 0
        else
          remaining_space / grow_child_count, Int.rem remaining_space grow_child_count
      in

      let children =
        List.folding_map rect.children ~init:0 ~f:(fun grow_index child ->
          let grow_index, new_span =
            match sizing_for_axis child axis with
            | Grow ->
              let extra_remainder = if grow_index < remainder then 1 else 0 in
              grow_index + 1, node_inner_span child axis + extra_per_grow_child + extra_remainder
            | _ -> grow_index, node_inner_span child axis
          in

          let child =
            match axis with
            | XAxis -> { child with width = new_span }
            | YAxis -> { child with height = new_span }
          in
          grow_index, grow_sizing child
        )
      in

      { node with kind = Container { rect with children } }
  in

  let stretch_cross_axis axis node =
    match node.kind with
    | Text _ -> node
    | Container rect ->
      let parent_span = node_inner_span node axis in
      let children =
        List.map rect.children ~f:(fun child ->
          let new_span =
            match sizing_for_axis child axis with
            | Grow -> max parent_span (node_inner_span child axis)
            | _ -> node_inner_span child axis
          in

          let child =
            match axis with
            | XAxis -> { child with width = new_span }
            | YAxis -> { child with height = new_span }
          in
          grow_sizing child
        )
      in
      { node with kind = Container { rect with children } }
  in

  (* TODO something for growing the parent node? *)
  match node.layout with
  | Horizontal -> node |> grow_main_axis XAxis |> stretch_cross_axis YAxis
  | Vertical -> node |> grow_main_axis YAxis |> stretch_cross_axis XAxis
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
  | Container rect ->
    let children =
      List.folding_map
        rect.children
        ~init:(~x:node.x_position, ~y:node.y_position)
        ~f:(fun (~x, ~y) child ->
          (* layout *)
          match node.layout with
          | Vertical ->
            let child = { child with x_position = x; y_position = y } in
            let y = y + node_outer_span child YAxis in
            (~x, ~y), child
          | Horizontal ->
            let child = { child with x_position = x; y_position = y } in
            let x = x + node_outer_span child XAxis in
            (~x, ~y), child
      )
      |> List.map ~f:(fun child -> calculate_positions child)
    in

    { node with kind = Container { rect with children } }
;;

let rec render node =
  match node.kind with
  | Container rect ->
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

let draw root = root |> fit_sizing |> grow_sizing |> calculate_positions |> render

let init ?(window_width=1200) ?(window_height=800) =
