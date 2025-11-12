include Ocaml_intrinsics_kernel.Int64

(** [deposit_bits a mask]: Deposit contiguous low bits from unsigned 64-bit integer a to
    dst at the corresponding bit locations specified by mask; all other bits in dst are
    set to zero. See [_pdep_u64]. *)
external deposit_bits
  :  int64
  -> int64
  -> int64
  @@ portable
  = "caml_bmi2_int64_deposit_bits_bytecode" "caml_bmi2_int64_deposit_bits"
[@@noalloc] [@@unboxed]

(** [extract_bits a mask]: Extract bits from unsigned 64-bit integer a at the
    corresponding bit locations specified by mask to contiguous low bits in dst; the
    remaining upper bits in dst are set to zero. See [_pext_u64]. *)
external extract_bits
  :  int64
  -> int64
  -> int64
  @@ portable
  = "caml_bmi2_int64_extract_bits_bytecode" "caml_bmi2_int64_extract_bits"
[@@noalloc] [@@unboxed]

(** Intrinsics for unboxed types. *)
module Unboxed = struct
  (** See [Stdlib.Float.bits_of_float] *)
  external bits_of_float
    :  (float#[@unboxed])
    -> (int64#[@unboxed])
    = "caml_int64_bits_of_float" "caml_int64_bits_of_float_unboxed"
  [@@noalloc]

  (** See [Stdlib.Float.float_of_bits] *)
  external float_of_bits
    :  (int64#[@unboxed])
    -> (float#[@unboxed])
    = "caml_int64_float_of_bits" "caml_int64_float_of_bits_unboxed"
  [@@noalloc]

  (** See [Ocaml_intrinsics_kernel.Int64]. *)

  external count_leading_zeros
    :  (int64#[@unboxed])
    -> (int64#[@unboxed])
    = "caml_int64_clz" "caml_int64_clz_unboxed_to_untagged"
  [@@noalloc]

  external count_leading_zeros_nonzero_arg
    :  (int64#[@unboxed])
    -> (int64#[@unboxed])
    = "caml_int64_clz" "caml_int64_clz_nonzero_unboxed_to_untagged"
  [@@noalloc]

  external count_trailing_zeros
    :  (int64#[@unboxed])
    -> (int64#[@unboxed])
    = "caml_int64_ctz" "caml_int64_ctz_unboxed_to_untagged"
  [@@noalloc]

  external count_trailing_zeros_nonzero_arg
    :  (int64#[@unboxed])
    -> (int64#[@unboxed])
    = "caml_int64_ctz" "caml_int64_ctz_nonzero_unboxed_to_untagged"
  [@@noalloc]

  external count_set_bits
    :  (int64#[@unboxed])
    -> (int64#[@unboxed])
    = "caml_int64_popcnt" "caml_int64_popcnt_unboxed_to_untagged"
  [@@noalloc]
end
