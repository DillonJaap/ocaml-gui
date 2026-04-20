open Core

(* returns the minimum int in a list, if the list is empty return 0 *)
let min_int list =
  match list with
  | [] -> 0
  | _ ->
    List.fold list ~init:Int.max_value ~f:(fun acc cur ->
      if cur < acc then cur else acc)
;;

let ratio ?(case_sensitive = false) from_text to_text =
  let from_bytes, to_bytes =
    if case_sensitive then
      from_text |> Bytes.of_string, to_text |> Bytes.of_string
    else
      ( from_text |> String.lowercase |> Bytes.of_string
      , to_text |> String.lowercase |> Bytes.of_string )
  in

  let tabulated_matrix =
    Array.make_matrix
      ~dimx:(Bytes.length from_bytes + 1)
      ~dimy:(Bytes.length to_bytes + 1)
      0
  in

  (* initialize 1..(length) values in first row and column *)
  Array.iteri tabulated_matrix ~f:(fun i _ -> tabulated_matrix.(i).(0) <- i);
  Array.iteri tabulated_matrix.(0) ~f:(fun i _ -> tabulated_matrix.(0).(i) <- i);

  for i = 1 to Array.length tabulated_matrix - 1 do
    for j = 1 to Array.length tabulated_matrix.(0) - 1 do
      let res =
        let open Char.O in
        match Bytes.get from_bytes (i - 1) = Bytes.get to_bytes (j - 1) with
        | true -> tabulated_matrix.(i - 1).(j - 1)
        | false ->
          let insert = tabulated_matrix.(i).(j - 1) + 1 in
          let delete = tabulated_matrix.(i - 1).(j) + 1 in
          let replace = tabulated_matrix.(i - 1).(j - 1) + 1 in
          min_int [ insert; delete; replace ]
      in
      tabulated_matrix.(i).(j) <- res
    done
  done;

  let edit_distance =
    tabulated_matrix.(Bytes.length from_bytes).(Bytes.length to_bytes)
  in

  let sum_of_lengths = Bytes.length from_bytes + Bytes.length to_bytes in
  float_of_int (sum_of_lengths - edit_distance) /. float_of_int sum_of_lengths
;;
