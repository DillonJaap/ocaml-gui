open Gui
open Core
open Raylib

type msg = UserClickedButton
type model = { color : Color.t }

let view (model : model) =
  (* close window and exit loop *)

  (* draw tree *)
  let open UI in
  container
    ~layout:Horizontal
    ~x_position:100
    ~y_position:20
    ~width:Fill
    ~height:Fill
    ~color:Color.darkpurple
    [ container
        ~color:Color.purple
        ~padding_all:10
        [ text ~text_color:Color.gold "this is an item in a list" ]
    ; container
        ~color:model.color
        ~padding:{ left = 20; right = 20; top = 20; bottom = 20 }
        ~on_click:UserClickedButton
        [ text ~text_color:Color.gold "This should be clickable" ]
    ; container
        ~color:Color.purple
        ~padding_all:10
        [ text ~text_color:Color.gold ~padding_all:4 "short text" ]
    ]
;;

let update msg model =
  match msg with
  | UserClickedButton -> { color = Color.green }
;;

let () =
  UI.start
    ~init:(fun () ->
      print_endline "we are init in the start function";
      { color = Color.purple }
    )
    ~update
    ~view
    ()
;;
