open Core

(* mutable text editor datastructure interface *)
module type TextEditor = sig
  type 'a t

  val get_cursor_position : unit -> int * int

  (* set *)
  val set_cursor_position : int -> int -> unit
  val set_cursor_x_position : int -> unit
  val set_cursor_y_position : int -> unit

  (* move by relative to current position*)
  val move_cursor_position : int -> int -> unit

  (* editing *)
  val get_text : 'a t -> string
  val insert : 'a t -> string -> int -> int -> unit
  val delete : 'a t -> unit
end

module IntArray = struct
  (** inserts the [to_insert] array at the specified position [pos] in the [array]*)
  let insert array to_insert pos =
    if pos = Array.length !array then
      array := Array.append !array to_insert
    else (
      (* new array of the needed size *)
      let new_array = Array.create 0 ~len:(Array.length !array + Array.length to_insert) in

      (* insert begining of array into our new array *)
      Array.blit ~src:!array ~dst:new_array ~src_pos:0 ~dst_pos:0 ~len:pos;

      (* insert our 'to_insert' text into the middle of the new array*)
      Array.blit ~src:to_insert ~dst:new_array ~src_pos:0 ~dst_pos:pos ~len:(Array.length to_insert);

      (* insert end of array into our new array *)
      Array.blit
        ~src:!array
        ~dst:new_array
        ~src_pos:pos
        ~dst_pos:(pos + Array.length to_insert)
        ~len:(Array.length !array - pos);

      (* update array ref to reference our new array *)
      array := new_array
    )
  ;;

  let delete text start_pos end_pos =
    let len = Array.length !text in
    let amount_to_delete = end_pos - start_pos + 1 in

    let new_data = Array.create 0 ~len:(len - amount_to_delete) in
    Array.blit ~src:!text ~dst:new_data ~src_pos:0 ~dst_pos:0 ~len:start_pos;
    Array.blit
      ~src:!text
      ~dst:new_data
      ~src_pos:(end_pos + 1)
      ~dst_pos:start_pos
      ~len:(len - end_pos - 1);
    text := new_data
  ;;

  (** Converts an [array] into a C array *)
  let to_carray array =
    let list = List.of_array array in
    Ctypes.CArray.of_list Ctypes.int list
  ;;

  let to_string codepoints =
    let open Ctypes in
    let byte_size = allocate int 0 in
    Array.fold_right codepoints ~init:"" ~f:(fun cp acc ->
      Raylib.codepoint_to_utf8 cp byte_size ^ acc
    )
  ;;
end
