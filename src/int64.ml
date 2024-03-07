include Ocaml_intrinsics_kernel.Int64

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
