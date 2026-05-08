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

module TextEditor_Bytes = struct
  type data =
    { mutable cursor_position : int
    ; mutable text : Buffer.t
    }

  let insert data text pos =
    let start = Buffer.sub data.text ~pos:0 ~len:pos in
    let end_ = Buffer.sub data.text ~pos ~len:(Buffer.length data.text - 1) in
    data.text
    <- Buffer.create
         (Bytes.length start + String.length text + Bytes.length end_);
    Buffer.add_bytes data.text start;
    Buffer.add_string data.text text;
    Buffer.add_bytes data.text end_
  ;;
end
