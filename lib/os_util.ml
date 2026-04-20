let readdir_option dir_handle =
  match Unix.readdir dir_handle with
  | str -> Some str
  | exception End_of_file -> None
;;

let ls dir =
  let dir_handle = Unix.opendir dir in
  let rec loop files =
    match readdir_option dir_handle with
    (* ignore relative directories *)
    | Some file when file = "." || file = ".." -> loop files
    | Some file -> loop (file :: files)
    | None -> files
  in
  let files = loop [] in
  Unix.closedir dir_handle;
  files
;;

let find_git_dirs starting_dir =
  let rec aux cur_dir git_dirs_acc =
    let files = ls cur_dir in

    let has_git_dir =
      files
      |> List.exists (fun file ->
        file = ".git" && (Unix.stat (cur_dir ^ "/" ^ file)).st_kind = Unix.S_DIR)
    in

    if has_git_dir then
      cur_dir :: git_dirs_acc
    else begin
      files
      |> List.fold_left
           begin fun acc dir ->
             let full_dir = cur_dir ^ "/" ^ dir in
             print_endline full_dir;

             if (Unix.stat full_dir).st_kind = Unix.S_DIR then
               aux full_dir acc
             else
               acc
           end
           git_dirs_acc
    end
  in
  aux starting_dir []
;;
