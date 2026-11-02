(** Exclusive temporary fixtures and session-isolated native helpers. *)

val with_directory : (string -> 'a) -> 'a
(** Run with an exclusive directory and remove the fixture afterwards. *)

val read : string -> string
(** Read a fixture's complete bytes. *)

val write : string -> string -> unit
(** Write a fixture's complete bytes. *)

val spawn :
  input:Unix.file_descr -> output:string -> string -> string list -> int
(** Start a native helper in a fresh session, redirecting its standard streams.
*)

val wait : int -> Unix.process_status
(** Reap the exact owned helper. *)

val run : output:string -> string -> string list -> Unix.process_status * string
(** Run a helper with closed input and collect its exit observation and output.
*)
