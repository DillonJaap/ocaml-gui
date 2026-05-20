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

type rectangle_type =
  { color : Raylib.Color.t
  ; children : node list
  }

and node_kind =
  | Rectangle of rectangle_type
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

let node_extent node axis =
  match axis with
  | XAxis -> node.width + node.padding.left + node.padding.right
  | YAxis -> node.height + node.padding.top + node.padding.bottom
;;

let node_span node axis =
  match axis with
  | XAxis -> node.width
  | YAxis -> node.height
;;

let node_sizing node axis =
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
  ; layout
  ; kind = Rectangle { color; children }
  }
;;

let empty () = rectangle []

let rec fit_sizing node =
  let sum_axis children axis = List.sum (module Int) children ~f:(fun c -> node_extent c axis) in

  let max_axis children axis =
    List.fold children ~init:0 ~f:(fun acc c -> max acc (node_extent c axis))
  in

  match node.kind with
  | Text text ->
    let size = measure_text_ex text.font text.content (float_of_int text.font_size) 0.0 in
    { node with width = Vector2.x size |> int_of_float; height = Vector2.y size |> int_of_float }
  | Rectangle rect ->
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

    { node with width; height; kind = Rectangle { rect with children } }
;;

let rec grow_sizing node : node =
  let grow_align_axis axis node =
    match node.kind with
    | Text _ -> node
    | Rectangle rect ->
      let remaining_space =
        List.fold rect.children ~init:0 ~f:(fun acc child -> acc + node_extent child axis)
      in
      let num_grow_containers =
        List.fold rect.children ~init:0 ~f:(fun acc child ->
          match node_sizing child axis with
          | Grow -> acc + 1
          | _ -> acc
        )
      in
      let grow_by = remaining_space / num_grow_containers in
      let remainder = Int.rem remaining_space num_grow_containers in

      let children =
        List.mapi rect.children ~f:(fun i child ->
          let span =
            if i < remainder then
              (* space divides unevenly so add 1 pixel until we have no more remainder *)
              node_span child axis + grow_by + 1
            else
              node_span child axis + grow_by
          in
          match axis with
          | XAxis -> { child with width = span }
          | YAxis -> { child with height = span }
        )
      in
      { node with kind = Rectangle { rect with children } }
  in

  let grow_across_axis axis node =
    match node.kind with
    | Text _ -> node
    | Rectangle rect ->
      let parent_span = node_span node axis in
      let children =
        List.map rect.children ~f:(fun child ->
          let span =
            match node_sizing child axis with
            | Grow -> max parent_span (node_span child axis)
            | _ -> node_span child axis
          in
          match axis with
          | XAxis -> { child with width = span }
          | YAxis -> { child with height = span }
        )
      in
      { node with kind = Rectangle { rect with children } }
  in

  (* TODO something for growing the parent node? *)
  match node.layout with
  | Horizontal -> node |> grow_align_axis XAxis |> grow_across_axis YAxis
  | Vertical -> node |> grow_align_axis YAxis |> grow_across_axis XAxis
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
          match node.layout with
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
