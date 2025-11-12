include Ocaml_intrinsics_kernel.Nativeint

(** Intrinsics for unboxed types. See [Ocaml_intrinsics_kernel.Nativeint]. *)
module Unboxed = struct
  external count_leading_zeros
    :  (nativeint#[@unboxed])
    -> (nativeint#[@unboxed])
    = "caml_nativeint_clz" "caml_nativeint_clz_unboxed_to_untagged"
  [@@noalloc]

  external count_leading_zeros_nonzero_arg
    :  (nativeint#[@unboxed])
    -> (nativeint#[@unboxed])
    = "caml_nativeint_clz" "caml_nativeint_clz_nonzero_unboxed_to_untagged"
  [@@noalloc]

  external count_trailing_zeros
    :  (nativeint#[@unboxed])
    -> (nativeint#[@unboxed])
    = "caml_nativeint_ctz" "caml_nativeint_ctz_unboxed_to_untagged"
  [@@noalloc]

  external count_trailing_zeros_nonzero_arg
    :  (nativeint#[@unboxed])
    -> (nativeint#[@unboxed])
    = "caml_nativeint_ctz" "caml_nativeint_ctz_nonzero_unboxed_to_untagged"
  [@@noalloc]

  external count_set_bits
    :  (nativeint#[@unboxed])
    -> (nativeint#[@unboxed])
    = "caml_nativeint_popcnt" "caml_nativeint_popcnt_unboxed_to_untagged"
  [@@noalloc]
end
