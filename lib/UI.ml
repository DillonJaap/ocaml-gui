open Raylib
open Core

type msg = UserClickedColorSwap

module Event = struct
  type 'a t = 'a
  type 'a event_queue = 'a t Queue.t
end

module UI = struct
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

  type 'a container_type =
    { color : Raylib.Color.t
    ; children : 'a node list
    ; on_click : 'a option
    }

  and 'a node_kind =
    | Container of 'a container_type
    | Text of
        { content : string
        ; font_size : int
        ; text_color : Raylib.Color.t
        ; font : Font.t
        }

  and 'a node =
    { width_sizing : sizing
    ; height_sizing : sizing
    ; width : int
    ; height : int
    ; x_position : int
    ; y_position : int
    ; layout : direction
    ; padding : padding
    ; kind : 'a node_kind
    ; id : string option
    }

  type axis_sizing =
    { total : int
    ; size : int
    ; pad_before : int
    ; pad_after : int
    }

  (** [node_outer_span node axis] get the sizing of the [node] along the specified [axis] plus the padding *)
  let node_outer_span node axis =
    match axis with
    | XAxis -> node.width + node.padding.left + node.padding.right
    | YAxis -> node.height + node.padding.top + node.padding.bottom
  ;;

  (** [node_inner_span node axis] get the sizing of the [node] along the specified [axis] not including the padding *)
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

  let rec grow_sizing node : 'a node =
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

  let calculate_tree root = root |> fit_sizing |> grow_sizing |> calculate_positions
end

let rec handle_mouse node =
  let open UI in
  let is_mouse_in_container node =
    let mx = get_mouse_x () in
    let my = get_mouse_y () in

    let left = node.x_position in
    let right = node.x_position + node.width in
    let top = node.y_position in
    let bottom = node.y_position + node.height in

    mx >= left && mx <= right && my >= top && my <= bottom
  in

  match node.kind, node.id with
  | Container container, Some id ->
    if is_mouse_button_pressed MouseButton.Left then
      if is_mouse_in_container node then
        printf "Wow you Inside clicked: %s\n" id
      else
        print_endline "you clicked nothing :("
  | _, _ ->
    ();

    ( match node.kind with
      | Container container -> List.iter container.children ~f:(fun child -> handle_mouse child)
      | Text _ -> ()
    )
;;

open UI

let draw root =
  let tree = calculate_tree root in
  handle_mouse tree;
  render tree
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
  ; id = None
  }
;;

let container
      ?id
      ?(layout = Horizontal)
      ?(x_position = 0)
      ?(y_position = 0)
      ?(width = Fill)
      ?(height = Fill)
      ?(padding = { left = 0; right = 0; top = 0; bottom = 0 })
      ?padding_all
      ?(color = Color.blank)
      ?on_click
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
  ; kind = Container { color; children; on_click }
  ; id
  }
;;

let empty () = container []

let start ?(window_width = 1200) ?(window_height = 800) ~init ~update ~view () =
  init_window window_width window_height "Project Launcher";

  (* theoretically center the window *)
  set_window_position
    ((get_screen_width () - window_width) / 2)
    ((get_screen_height () - window_height) / 2);

  (* target FPS *)
  set_target_fps 60;

  (* init *)
  let model = init () in

  (* loop *)
  let rec loop model =
    (* close window and exit loop *)
    if window_should_close () then (
      close_window ();
      exit 0
    );

    begin_drawing ();
    view model |> draw;
    let model = update NoMsg model in
    end_drawing ();

    loop model
  in
  loop model
;;
