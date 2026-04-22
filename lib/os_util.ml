open Core

let ls dir = Stdlib.Sys.readdir dir |> Array.to_list

let find_git_dirs starting_dir =
  let rec aux cur_dir git_dirs_acc =
    let files = ls cur_dir in

    let has_git_dir =
      List.exists files ~f:begin fun file ->
          String.(file = ".git")
          && Stdlib.Sys.is_directory (cur_dir ^ "/" ^ file)
        end
    in

    if has_git_dir then
      cur_dir :: git_dirs_acc
    else
      List.fold files ~init:git_dirs_acc ~f:begin fun acc dir ->
          let full_dir = cur_dir ^ "/" ^ dir in
          print_endline full_dir;
          if Stdlib.Sys.is_directory full_dir then
            aux full_dir acc
          else
            acc
        end
  in
  aux starting_dir []
;;
