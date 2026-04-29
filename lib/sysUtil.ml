let ls dir = Stdlib.Sys.readdir dir |> Array.to_list

let file_exists_and_is_dir file =
  Stdlib.Sys.file_exists file && Stdlib.Sys.is_directory file
;;

let find_git_dirs starting_dir =
  let open Core in
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
          (* print_endline full_dir; *)
          if Stdlib.Sys.is_directory full_dir then
            aux full_dir acc
          else
            acc
        end
  in
  aux starting_dir []
;;

let daemonize ~prog ~argv =
  (* First fork *)
  match Core_unix.fork () with
  | `In_the_child ->
    (* Create new session: detach from controlling terminal *)
    ignore (Unix.setsid ());
    (* Second fork: ensures we're not a session leader,
       so we can never re-acquire a terminal *)
    ( match Core_unix.fork () with
      | `In_the_child ->
        (* Redirect stdio to /dev/null *)
        let devnull = Unix.openfile "/dev/null" [ Unix.O_RDWR ] 0 in
        Unix.dup2 devnull Unix.stdin;
        Unix.dup2 devnull Unix.stdout;
        Unix.dup2 devnull Unix.stderr;
        Unix.close devnull;
        Unix.execv prog argv
      | `In_the_parent pid ->
        (* Intermediate child exits immediately *)
        ignore pid;
        exit 0
    )
  | `In_the_parent pid ->
    (* Wait for intermediate child to exit *)
    ( try ignore (Unix.waitpid [] (Core.Pid.to_int pid)) with
      | Unix.Unix_error (Unix.ECHILD, _, _) -> ()
    );
    ()
;;
