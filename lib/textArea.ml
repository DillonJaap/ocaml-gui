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

module TextArea_Buffer = struct
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

  let delete data start_pos end_pos =
    let start = Buffer.sub data.text ~pos:0 ~len:(start_pos - 1) in
    let end_ =
      Buffer.sub data.text ~pos:(end_pos + 1) ~len:(Buffer.length data.text - 1)
    in
    data.text <- Buffer.create (Bytes.length start + Bytes.length end_);
    Buffer.add_bytes data.text start;
    Buffer.add_bytes data.text end_
  ;;
end

module TextArea_UChar = struct
  type data =
    { mutable cursor_position : int
    ; mutable text : Uchar.t array
    }

  let insert data text pos =
    if pos = Array.length data.text - 1 then
      data.text <- Array.append data.text text
    else begin
      let start = Array.sub data.text ~pos:0 ~len:pos in
      let end_ = Array.sub data.text ~pos ~len:(Array.length data.text - 1) in

      data.text
      <- Array.create ~len:0 (Uchar.of_char 'c')
         |> Array.append start
         |> Array.append text
         |> Array.append end_
    end
  ;;

  let delete data start_pos end_pos =
    let start = Array.sub data.text ~pos:0 ~len:(start_pos - 1) in
    let end_ =
      Array.sub data.text ~pos:(end_pos + 1) ~len:(Array.length data.text - 1)
    in

    data.text
    <- Array.create ~len:0 (Uchar.of_char 'c')
       |> Array.append start
       |> Array.append end_
  ;;
end
