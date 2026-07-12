open Gui
open Core
open Raylib

type msg = UserClickedButton

let element_list config =
  let rec loop () =
    (* close window and exit loop *)
    if window_should_close () then (
      close_window ();
      exit 0
    );

    begin_drawing ();
    clear_background Color.darkgray;

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
          ~id:"foo"
          ~color:Color.purple
          ~padding:{ left = 20; right = 20; top = 20; bottom = 20 }
          ~on_click:UserClickedButton
          [ text ~text_color:Color.gold "This should be clickable" ]
      ; container
          ~color:Color.purple
          ~padding_all:10
          [ text ~text_color:Color.gold ~padding_all:4 "short text" ]
      ]
    |> draw;

    end_drawing ();
    loop ()
  in
  loop ()
;;

let () =
  UI.init ();
  let config = Config.initialize () in
  (* element_list config *)
  element_list config
;;
