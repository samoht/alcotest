(** Test identities, outcomes and registered suites. *)

open! Import

type speed_level = [ `Quick | `Slow ]

module Test_name : sig
  type t

  val v : name:string -> index:int -> t
  (** Construct an escaped test-case identity and its output filename. *)

  val name : t -> Safe_string.t
  (** Return the escaped and original forms of the test name. *)

  val index : t -> int
  (** Return the test index within its named group. *)

  val pp : t Fmt.t
  (** Pretty-print the unescaped test-case name. *)

  val file : t -> string
  (** An escaped form of the test name with [.output] suffix. *)

  val length : t -> int
  (** The approximate number of terminal columns consumed by [pp_name]. *)

  val compare : t -> t -> int
  (** Order lexicographically by name, then by index. *)
end

module Run_result : sig
  type t =
    [ `Ok
    | `Exn of Test_name.t * string * unit Fmt.t
    | `Error of Test_name.t * unit Fmt.t
    | `Skip
    | `Todo of string ]

  val is_failure : t -> bool
  (** Recognize failed, exceptional and unfinished test results. *)
end

module Suite (M : Monad.S) : sig
  type 'a t
  type 'a test_fn = [ `Skip | `Run of 'a -> Run_result.t M.t ]

  type 'a test_case = {
    name : Test_name.t;
    speed_level : speed_level;
    fn : 'a test_fn;
  }

  val v : name:string -> (_ t, [> `Empty_name ]) result
  (** Construct a new suite, given a non-empty [name]. Test cases must be added
      with {!add}. *)

  val name : _ t -> string
  (** An escaped form of the suite name. *)

  val pp_name : _ t Fmt.t
  (** Pretty-print the unescaped suite name. *)

  val add :
    'a t ->
    Test_name.t * string * speed_level * 'a test_fn ->
    ('a t, [ `Duplicate_test_path of string ]) result
  (** Register a test or report its duplicate output-file identity. *)

  val tests : 'a t -> 'a test_case list
  (** Return registered test cases in execution order. *)

  val doc_of_test_name : 'a t -> Test_name.t -> string
  (** Return the description registered for the given test identity. *)
end
