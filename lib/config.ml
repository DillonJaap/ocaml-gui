open Sexplib.Std

type config = { font_size : int [@sexp.name "font-size"] } [@@deriving sexp]

let load_config = config_of_sexp (Sexplib.Sexp.of_string "((font_size 48))")
