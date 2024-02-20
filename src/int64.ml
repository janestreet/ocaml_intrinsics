(** [count_leading_zeros n] returns the number of most-significant
    zero bits before the most significant set bit in [n].
    If [n] is 0, the result is the number of bits in [n],
    that is 64. *)
external count_leading_zeros
  :  (int64[@unboxed])
  -> (int[@untagged])
  = "caml_int64_clz" "caml_int64_clz_unboxed_to_untagged"
  [@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]

(** Same as [count_leading_zeros] except if the argument is zero,
    then the result is undefined. Emits more efficient code. *)
external count_leading_zeros_nonzero_arg
  :  (int64[@unboxed])
  -> (int[@untagged])
  = "caml_int64_clz" "caml_int64_clz_nonzero_unboxed_to_untagged"
  [@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]

(** [count_trailing_zeros n] returns the number of least-significant
    zero bits before the least significant set bit in [n].
    If [n] is 0, the result is the number of bits in [n],
    that is 64. *)
external count_trailing_zeros
  :  (int64[@unboxed])
  -> (int[@untagged])
  = "caml_int64_ctz" "caml_int64_ctz_unboxed_to_untagged"
  [@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]

(** Same as [count_trailing_zeros] except if the argument is zero,
    then the result is undefined. Emits more efficient code. *)
external count_trailing_zeros_nonzero_arg
  :  (int64[@unboxed])
  -> (int[@untagged])
  = "caml_int64_ctz" "caml_int64_ctz_nonzero_unboxed_to_untagged"
  [@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]

(** [count_set_bits n] returns the number of bits that are 1 in [n]. *)
external count_set_bits
  :  (int64[@unboxed])
  -> (int[@untagged])
  = "caml_int64_popcnt" "caml_int64_popcnt_unboxed_to_untagged"
  [@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]

(** [deposit_bits a mask]: Deposit contiguous low bits from unsigned 64-bit
    integer a to dst at the corresponding bit locations specified by mask;
    all other bits in dst are set to zero. See [_pdep_u64]. *)
external deposit_bits
  :  int64
  -> int64
  -> int64
  = "caml_bmi2_int64_deposit_bits_bytecode" "caml_bmi2_int64_deposit_bits"
  [@@noalloc] [@@unboxed] [@@builtin] [@@no_effects] [@@no_coeffects]

(** [extract_bits a mask]: Extract bits from unsigned 64-bit integer a at the
    corresponding bit locations specified by mask to contiguous low bits in dst;
    the remaining upper bits in dst are set to zero. See [_pext_u64]. *)
external extract_bits
  :  int64
  -> int64
  -> int64
  = "caml_bmi2_int64_extract_bits_bytecode" "caml_bmi2_int64_extract_bits"
  [@@noalloc] [@@unboxed] [@@builtin] [@@no_effects] [@@no_coeffects]
