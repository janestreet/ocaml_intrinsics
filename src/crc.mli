external int_crc
  :  initial:int
  -> data:int
  -> int
  = "caml_sse42_int_untagged_crc_bytecode" "caml_sse42_int_untagged_crc"
  [@@noalloc] [@@untagged] [@@builtin] [@@no_effects] [@@no_coeffects]

external int64_crc
  :  initial:(int[@untagged])
  -> data:(int64[@unboxed])
  -> (int[@untagged])
  = "caml_sse42_int64_crc_bytecode" "caml_sse42_int64_crc"
  [@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]

(** Accumulates [iterations] of [int_crc].
    If [iterations] < 0, raises Invalid_argument. *)
val iterated_crc_exn : initial:int -> iterations:int -> data:int -> int
