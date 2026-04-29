(* mutable text editor datastructure interface *)
module type TextEditor = sig
  type 'a t

  val cursor_position : unit -> int * int
  val set_cursor_position : int -> int -> unit
  val set_cursor_x_position : int -> unit
  val set_cursor_y_position : int -> unit
end
