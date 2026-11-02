(** This module exists for internal Alcotest use only. It provides no stability
    guarantee. *)

val ( >> ) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c
(** [f >> g] applies [f] followed by [g]. *)

module Fun : sig
  val id : 'a -> 'a
  (** [id x] returns [x]. *)
end

module Int : sig
  module Set : Set.S with type elt = int
end

module String : sig
  include module type of Astring.String

  val length_utf8 : string -> int
  (** Get the length of a string in UTF-8 characters and malformed segments. *)

  val prefix_utf8 : int -> string -> string
  (** [prefix_utf8 n s] is the prefix of [s] containing [n] UTF-8 characters (or
      [s] if it contains fewer than [n] UTF-8 characters). *)
end

module List : sig
  include module type of List

  type 'a t = 'a list

  val rev_head : int -> 'a list -> 'a list
  (** Reverse a list, taking at most the first n elements of the original list.
  *)

  val concat_map : ('a -> 'b list) -> 'a list -> 'b list
  (** [concat_map f xs] concatenates the lists returned by [f]. *)

  val filter_map : ('a -> 'b option) -> 'a list -> 'b list
  (** [filter_map f xs] retains the present values returned by [f]. *)

  val lift_result : ('a, 'b) result t -> ('a t, 'b t) result
  (** [lift_result xs] gathers successful values, or all errors in order. *)

  val init : int -> (int -> 'a) -> 'a list
  (** [init n f] builds the list of [f i] for indices from zero to [n - 1]. *)
end

module Result : sig
  val map : ('a -> 'b) -> ('a, 'e) result -> ('b, 'e) result
  (** [map f x] applies [f] to the successful or present value of [x]. *)
end

module Option : sig
  val map : ('a -> 'b) -> 'a option -> 'b option
  (** [map f x] applies [f] to the successful or present value of [x]. *)

  val bind : 'a option -> ('a -> 'b option) -> 'b option
  (** [bind x f] applies [f] to a present value and preserves absence. *)

  val is_some : _ option -> bool
  (** [is_some x] holds exactly when [x] contains a value. *)

  val get_exn : 'a option -> 'a
  (** [get_exn x] returns a present value or raises [Invalid_argument]. *)

  val value : default:'a -> 'a option -> 'a
  (** [value ~default x] returns a present value or [default]. *)

  val ( || ) : 'a option -> 'a option -> 'a option
  (** [a || b] returns the first present option, preferring [a]. *)
end

module Cmdliner_syntax : sig
  open Cmdliner

  val ( let+ ) : 'a Term.t -> ('a -> 'b) -> 'b Term.t
  (** The mapping operator applies a function to a command-line term. *)

  val ( and+ ) : 'a Term.t -> 'b Term.t -> ('a * 'b) Term.t
  (** The pairing operator combines the values of two command-line terms. *)

  val ( >>| ) : 'a Term.t -> ('a -> 'b) -> 'b Term.t
  (** [term >>| f] maps [f] over a command-line term. *)
end
