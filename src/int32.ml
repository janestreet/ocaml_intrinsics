(** [count_leading_zeros n] returns the number of most-significant
    zero bits before the most significant set bit in [n].
    If [n] is 0, the result is the number of bits in [n],
    that is 32. *)
external count_leading_zeros
  :  (int32[@unboxed])
  -> (int[@untagged])
  = "caml_int32_clz" "caml_int32_clz_unboxed_to_untagged"
[@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]

(** [count_trailing_zeros n] returns the number of least-significant
    zero bits before the least significant set bit in [n].
    If [n] is 0, the result is the number of bits in [n],
    that is 32. *)
external count_trailing_zeros
  :  (int32[@unboxed])
  -> (int[@untagged])
  = "caml_int32_ctz" "caml_int32_ctz_unboxed_to_untagged"
[@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]

(** [count_set_bits n] returns the number of bits that are 1 in [n]. *)
external count_set_bits
  :  (int32[@unboxed])
  -> (int[@untagged])
  = "caml_int32_popcnt" "caml_int32_popcnt_unboxed_to_untagged"
[@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]
