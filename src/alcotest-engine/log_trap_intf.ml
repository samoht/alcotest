open! Import
open Model

(** Captured output belongs to a physical execution directory, with one
    [.output] file per test case. A platform with complete-run ownership wraps
    the full execution and report in {!with_run}; the Unix backend uses leases
    and retained latest/failure references to retire only registered completed
    output. The shipped preparation-only maker remains available as an unmanaged
    compatibility constructor. Diagnostic paths always name the physical
    execution, even while a concurrent run replaces public aliases. *)

module type S = sig
  type 'a promise
  type t

  val inactive : t
  val active : root:string -> uuid:string -> suite_name:string -> t

  val with_run : t -> (unit -> int promise) -> int promise
  (** Own captured logs for the complete run and report its failure count. *)

  val with_captured_logs :
    t -> Test_name.t -> ('a -> 'b promise) -> 'a -> 'b promise
  (** Capture all logs for a given test run. *)

  val recover_logs :
    t ->
    tail:[ `Unlimited | `Limit of int ] ->
    Test_name.t ->
    (Format.formatter -> unit) option
  (** Print the logs for a given test to the given formatter, if they exist.
      [tail] determines whether to show all lines in the captured log or just a
      suffix of them. *)

  val pp_current_run_dir : t -> Format.formatter -> unit
  (** Print the folder containing all captured traces for the current test run.
      Raises an exception if traces are not being recorded. *)

  val pp_log_location : t -> Test_name.t -> Format.formatter -> unit
  (** Print the file containing the trace of a particular test. Raises an
      exception if traces are not being recorded. *)
end

module type Log_trap = sig
  module type S = S

  module Make
      (Promise : Monad.EXTENDED)
      (_ : Platform.S_with_log_trap with type 'a promise := 'a Promise.t) :
    S with type 'a promise := 'a Promise.t
end
