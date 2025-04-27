include Ocaml_intrinsics_kernel.Int32

(** Intrinsics for unboxed types. See [Ocaml_intrinsics_kernel.Int32]. *)
module Unboxed = struct
  external count_leading_zeros
    :  (int32#[@unboxed])
    -> (int32#[@unboxed])
    = "caml_int32_clz" "caml_int32_clz_unboxed_to_untagged"
  [@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]

  external count_leading_zeros_nonzero_arg
    :  (int32#[@unboxed])
    -> (int32#[@unboxed])
    = "caml_int32_clz" "caml_int32_clz_nonzero_unboxed_to_untagged"
  [@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]

  external count_trailing_zeros
    :  (int32#[@unboxed])
    -> (int32#[@unboxed])
    = "caml_int32_ctz" "caml_int32_ctz_unboxed_to_untagged"
  [@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]

  external count_trailing_zeros_nonzero_arg
    :  (int32#[@unboxed])
    -> (int32#[@unboxed])
    = "caml_int32_ctz" "caml_int32_ctz_nonzero_unboxed_to_untagged"
  [@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]

  external count_set_bits
    :  (int32#[@unboxed])
    -> (int32#[@unboxed])
    = "caml_int32_popcnt" "caml_int32_popcnt_unboxed_to_untagged"
  [@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]
end
