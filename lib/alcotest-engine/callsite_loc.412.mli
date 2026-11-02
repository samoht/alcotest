(** Best-effort caller locations for failed assertions. *)

val find : ?__FUNCTION__:string -> unit -> Lexing.position option
(** Infer a caller position when source-location reporting is enabled and the
    runtime supplies a suitable backtrace frame. *)
