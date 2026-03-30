include module type of Ocaml_intrinsics_kernel.Float

(** Rounds a [float] to an integer [float] using the current rounding mode. In native
    code, the default rounding mode is "round half to even", and we expect that no program
    will change the rounding mode. *)
val round_half_to_even : float -> float

(** Rounds a [float] to an [int64] using the current rounding mode. In native code, the
    default rounding mode is "round half to even", and we expect that no program will
    change the rounding mode.

    If the argument is NaN or infinite or if the rounded value cannot be represented, the
    result is unspecified. *)
val iround_half_to_even : float -> int64

module Unboxed : sig
  include module type of Unboxed

  val round_half_to_even : float -> float
  val iround_half_to_even : float -> int64
end
