(** Alcotest uses [Format.std_formatter] and [Format.err_formatter] formatters.
    However, in a parallel context (OCaml 5 and domains), using these values in
    parallel can lead to {i data-races}, since these values are not
    {i domains-safe}. As such, Alcotest offers a way to create your own
    formatter equivalent in behavior to [Format.std_formatter] and
    [Format.err_formatter] (i.e. they write well on [1] and [2]) but they can be
    used without risk even if another library (such as [Logs]) uses
    [Format.std_formatter] and/or [Format.err_formatter] and is used in
    parallel. *)

type stdout = private Format.formatter
type stderr = private Format.formatter

val set_stdout : stdout -> unit
(** Set the formatter used for runner standard output. *)

val set_stderr : stderr -> unit
(** Set the formatter used for runner standard error. *)

val get_stdout : unit -> stdout
(** Return the currently configured standard-output formatter. *)

val get_stderr : unit -> stderr
(** Return the currently configured standard-error formatter. *)

val ocaml_stdout : stdout
(** The process-wide OCaml standard-output formatter. *)

val ocaml_stderr : stderr
(** The process-wide OCaml standard-error formatter. *)

val make_stdout : unit -> stdout
(** Create an independent formatter writing to standard output. *)

val make_stderr : unit -> stderr
(** Create an independent formatter writing to standard error. *)

val pr : ('a, Format.formatter, unit) format -> 'a
(** Format a message on the currently configured standard-output formatter. *)

val epr : ('a, Format.formatter, unit) format -> 'a
(** Format a message on the currently configured standard-error formatter. *)
