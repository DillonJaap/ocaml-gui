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
  let insert text text_to_insert pos =
    if pos = Array.length !text - 1 then
      text := Array.append !text text_to_insert
    else begin
      let new_text =
        Array.create 0 ~len:(Array.length !text + Array.length text_to_insert)
      in
      Array.blit ~src:!text ~dst:new_text ~src_pos:0 ~dst_pos:0 ~len:pos;
      Array.blit
        ~src:text_to_insert
        ~dst:new_text
        ~src_pos:0
        ~dst_pos:pos
        ~len:(Array.length text_to_insert);
      Array.blit
        ~src:!text
        ~dst:new_text
        ~src_pos:pos
        ~dst_pos:(pos + Array.length text_to_insert)
        ~len:(Array.length !text - pos);
      text := new_text
    end
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

  let to_carray text =
    let list = List.of_array text in
    Ctypes.CArray.of_list Ctypes.int list
  ;;
end
