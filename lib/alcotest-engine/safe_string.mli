(** UTF-8 test names escaped for use as filesystem path segments. *)

type t
(** A filesystem name that replaces path separators with hyphens, escapes
    unsupported codepoints as [U+XXXX], and normalizes malformed UTF-8 to the
    Unicode replacement character. The original bytes remain available through
    {!to_unescaped_string}. *)

val v : string -> t
(** [v s] escapes unsafe characters while preserving the original string. *)

val to_string : t -> string
(** Get the escaped form of the given {!Safe_string}. *)

val to_unescaped_string : t -> string
(** Get the raw, unescaped string initially passed to {!val-v}. *)

val pp : t Fmt.t
(** Pretty-print the unescaped string. *)

val length : t -> int
(** The approximate number of terminal columns consumed by {!pp}. *)

val equal : t -> t -> bool
(** Compare escaped filesystem identities for equality. *)

val compare : t -> t -> int
(** Order escaped filesystem identities lexicographically. *)
