let readdir_option dir_handle =
  match Unix.readdir dir_handle with
  | str -> Some str
  | exception End_of_file -> None
;;

let ls dir =
  let dir_handle = Unix.opendir dir in
  let _ = Printf.sprintf "hello you are %s" "dillon" in

  let rec loop files =
    match readdir_option dir_handle with
    (* ignore relative directories *)
    | Some file when file = "." || file = ".." -> loop files
    | Some file -> loop (file :: files)
    | None -> files
  in
  loop []
;;
