(** Platform services and optional complete-run log ownership. *)

include module type of Platform_intf

(** Adapt a preparation-only platform without changing its lifetime contract. *)
module Legacy (P : MAKER) (M : Monad.S) :
  S_with_log_trap with type 'a promise := 'a M.t
