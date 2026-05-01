open Sexplib.Std
open Core

type launcher =
  { path : string
  ; args : string list
  }
[@@deriving sexp]

type config_section =
  { font_size : int [@sexp.name "font-size"]
  ; code_dir : string
  ; font_dir : string
  ; launchers : launcher list
  }
[@@deriving sexp]

type config_file =
  { global : config_section
  ; mac : config_section option
  ; linux : config_section option
  }
[@@deriving sexp_of]

let config_file_of_sexp sexp =
  let default_config =
    { global = { font_size = 36; code_dir = ""; font_dir = ""; launchers = [] }
    ; mac = None
    ; linux = None
    }
  in

  match sexp with
  | Sexp.List l ->
    List.fold l ~init:default_config ~f:(fun cfg cur ->
      match cur with
      | Sexp.List [ Sexp.Atom "mac"; mac_cfg ] ->
        { cfg with mac = Some (config_section_of_sexp mac_cfg) }
      | Sexp.List [ Sexp.Atom "linux"; linux_cfg ] ->
        { cfg with linux = Some (config_section_of_sexp linux_cfg) }
      | _ -> { cfg with global = config_section_of_sexp cur }
    )
  | _ -> raise (Sexp.Of_sexp_error (Failure "invalid shape", sexp))
;;

let parse_config_file () =
  let config_string =
    In_channel.read_all "/home/dillon/code/ocaml-gui/assets/config"
  in
  config_string |> Sexplib.Sexp.of_string |> config_file_of_sexp
;;
