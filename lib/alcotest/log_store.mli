(** Native log ownership. Only runs registered here are eligible for retirement;
    directories made by older runners remain outside this store's authority. *)

type t

val start : root:string -> uuid:string -> name:string -> t
(** [start] leases a new run and publishes its latest-suite alias. *)

val finish : t -> failed:bool -> unit
(** [finish] publishes failure diagnostics when needed, releases the lease and
    retires completed runs superseded by both retained references. *)

val prepare_legacy : root:string -> uuid:string -> name:string -> unit
(** Compatibility preparation for the shipped maker contract. These runs lack a
    completion callback and remain outside automatic retirement. *)
