(** JUnit XML reports. *)

type outcome =
  | Passed
  | Skipped
  | Failed of { kind : string; text : string }
      (** A failed [check] or [fail], rendered as [<failure>]. *)
  | Errored of { kind : string; text : string }
      (** Any other exception, or the process exiting during the test, rendered
          as [<error>]. *)

type case = {
  group : string;
  name : string;
  index : int;  (** The position of the test in its group. *)
  time : float;  (** In seconds. *)
  location : (string * int) option;  (** Where the test is defined. *)
  outcome : outcome;
  output : string option;  (** Rendered as [<system-out>]. *)
}

type t

val v : suite:string -> file:string option -> timestamp:string -> t
(** [v ~suite ~file ~timestamp] is the report of the suite [suite], run from the
    source file [file] and started at [timestamp], with no case. *)

val add : t -> case -> t

val to_string : t -> string
(** [to_string t] is the document for the cases of [t]. *)
