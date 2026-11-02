(** Native terminal dimensions. *)

type dimensions = { rows : int; columns : int }
(** Get the dimensions of the terminal *)

val dimensions : unit -> dimensions option
(** Return the dimensions of the current terminal, if available. *)
