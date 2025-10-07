@@ portable

(** Rounds a [float] to an integer [float] using the current rounding mode. The default
    rounding mode is "round half to even", and we expect that no program will change the
    rounding mode.

    On an x86-64 machine, this compiles to [roundsd xmm0, xmm1, $12]. Requires SSE4.1. On
    ARM, this calls a C implementation. *)
val round_half_to_even : float -> float

(** Rounds a [float] to an integer [float] using the mode specified in the function name.

    On an x86-64 machine, these compile to [roundsd xmm0, xmm1, $N]. Requires SSE4.1. On
    ARM, these call a C implementation. *)

val round_down : float -> float
val round_up : float -> float
val round_towards_zero : float -> float
val round_nearest : float -> float

include module type of Ocaml_intrinsics_kernel.Float

module Unboxed : sig
  include module type of Unboxed

  val round_half_to_even : float# -> float#
  val round_down : float# -> float#
  val round_up : float# -> float#
  val round_towards_zero : float# -> float#
  val round_nearest : float# -> float#
end
