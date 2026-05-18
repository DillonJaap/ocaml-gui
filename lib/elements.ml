open Raylib
open Core

type direction =
  | Vertical
  | Horizontal

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
      }

and node =
  { width : int
  ; height : int
  ; x_position : int
  ; y_position : int
  ; padding : padding
  ; kind : node_kind
  }

let text
      ?(font_size = 36)
      ?(padding = { left = 0; right = 0; top = 0; bottom = 0 })
      ~color:text_color
      text
  =
  { width = measure_text text font_size
  ; height = font_size
  ; padding
  ; x_position = 0
  ; y_position = 0
  ; kind = Text { content = text; font_size; text_color }
  }
;;

let rectangle
      ?(layout = Horizontal)
      ?(x_position = 0)
      ?(y_position = 0)
      ?(width = 0)
      ?(height = 0)
      ?(padding = { left = 0; right = 0; top = 0; bottom = 0 })
      ?(color = Color.blank)
      children
  =
  { width
  ; height
  ; padding
  ; x_position
  ; y_position
  ; kind = Rectangle { color; layout; children }
  }
;;

let rec calculate_sizes node =
  match node.kind with
  (* TODO calculate width and height of text here *)
  | Text _ -> node
  | Rectangle rect when List.length rect.children = 0 -> node
  | Rectangle rect ->
    let (~width, ~height), children =
      List.fold_map
        rect.children
        ~init:(~width:0, ~height:0)
        ~f:(fun (~width, ~height) child ->
          let child = calculate_sizes child in
          match rect.layout with
          | Vertical ->
            let height =
              height + child.height + child.padding.top + child.padding.bottom
            in
            let width =
              max (child.width + child.padding.left + child.padding.right) width
            in
            (~width, ~height), child
          | Horizontal ->
            let height =
              max
                (child.height + child.padding.top + child.padding.bottom)
                height
            in
            let width =
              width + child.width + child.padding.left + child.padding.right
            in
            (~width, ~height), child
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
  | Rectangle rect when List.length rect.children = 0 -> node
  | Rectangle rect ->
    let children =
      List.folding_map
        rect.children
        ~init:(~x:node.x_position, ~y:node.y_position)
        ~f:(fun (~x, ~y) child ->
          match rect.layout with
          | Vertical ->
            let child = { child with x_position = x; y_position = y } in
            let y =
              y + child.height + child.padding.top + child.padding.bottom
            in
            (~x, ~y), calculate_positions child
          | Horizontal ->
            let child = { child with x_position = x; y_position = y } in
            let x =
              x + child.width + child.padding.left + child.padding.right
            in
            (~x, ~y), calculate_positions child
      )
    in
    { node with kind = Rectangle { rect with children } }
;;

let rec render node =
  match node.kind with
  | Rectangle rect ->
    draw_rectangle
      node.x_position
      node.y_position
      node.width
      node.height
      rect.color;
    List.iter rect.children ~f:(fun child -> render child)
  | Text text ->
    draw_text
      text.content
      node.x_position
      node.y_position
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
