open Sexplib.Std
open Core

type launcher =
  { path : string
  ; args : string list
  }
[@@deriving sexp]

type config_file =
  { font_size : int [@sexp.name "font-size"]
  ; code_dir : string
  ; font_dir : string
  ; launchers : launcher list
  }
[@@deriving sexp]

let parse_config_file () =
  let config_string =
    In_channel.read_all "/home/dillon/code/ocaml-gui/assets/config"
  in
  config_file_of_sexp (Sexplib.Sexp.of_string config_string)
;;
