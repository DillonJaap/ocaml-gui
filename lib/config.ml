open Sexplib.Std
open Core

type launcher =
  { name : string
  ; args : string list
  }
[@@deriving sexp]

type config =
  { font_size : int [@sexp.name "font-size"]
  ; launchers : launcher list
  }
[@@deriving sexp]

let load_config () =
  let config_string =
    In_channel.read_all "/home/dillon/code/ocaml-gui/assets/config"
  in
  config_of_sexp (Sexplib.Sexp.of_string config_string)
;;
