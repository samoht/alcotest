(** Canonical execution identifiers shared by the runner and native log store.
*)

type t

val v : unit -> t
(** [v ()] generates a new execution identifier. *)

val of_string : string -> t option
(** [of_string s] accepts exactly the execution identifier format. *)

val to_string : t -> string
(** [to_string t] is the canonical path segment for [t]. *)
