(* mutable text editor interface *)
module type TextEditor = sig
  type 'a t

  val cursor_position : unit -> int * int
end
