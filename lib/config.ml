open Sexplib.Std
open Core
open Raylib

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
  print_endline "OS:";
  print_endline (Core_unix.Utsname.sysname (Core_unix.uname ()));

  let file_path =
    match Core_unix.Utsname.sysname (Core_unix.uname ()) with
    | "linux" -> "/home/dillon/code/ocaml-gui/assets/config"
    | "Darwin" -> "/Users/DJaap/code/personal/ocaml/gui/assets/config"
    | _ -> failwith "unknonw OS for config file path"
  in

  let config_string = In_channel.read_all file_path in
  config_string |> Sexplib.Sexp.of_string |> config_file_of_sexp
;;

type config =
  { font_size : int
  ; code_dir : string
  ; font : Font.t
  ; launchers : launcher list
  }

let initialize () =
  let config_file = parse_config_file () in
  print_s [%sexp (config_file : config_file)];

  let config_file =
    match Core_unix.Utsname.sysname (Core_unix.uname ()), config_file.mac, config_file.linux with
    | "linux", _, Some linux -> linux
    | "Darwin", Some mac, _ -> mac
    | _ -> config_file.global
  in

  (* set font *)
  let font = load_font config_file.font_dir in
  set_texture_filter (Font.texture font) TextureFilter.Trilinear;

  (* verifiy code_dir exists *)
  if not (SysUtil.file_exists_and_is_dir config_file.code_dir) then
    failwith (Printf.sprintf "no such code path directory: %s" config_file.code_dir);

  { font_size = config_file.font_size
  ; code_dir = config_file.code_dir
  ; font
  ; launchers = config_file.launchers
  }
;;
