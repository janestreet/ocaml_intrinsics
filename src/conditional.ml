(** [select_value c a b] is equivalent to [if c then a else b)]
    where [a] and [b] are eagerly evaluated, regardless of the value of [c].
    Compiles to CMOV instruction on amd64 targets.
    Can be used to avoid branch misprediction when [c] is data dependent. *)
external select_value : bool -> 'a -> 'a -> 'a = "caml_csel_value"
  [@@noalloc] [@@no_effects] [@@no_coeffects] [@@builtin]

external select_int
  :  bool
  -> (int[@untagged])
  -> (int[@untagged])
  -> (int[@untagged])
  = "caml_csel_value" "caml_csel_int_untagged"
  [@@noalloc] [@@no_effects] [@@no_coeffects] [@@builtin]

external select_int64
  :  bool
  -> (int64[@unboxed])
  -> (int64[@unboxed])
  -> (int64[@unboxed])
  = "caml_csel_value" "caml_csel_int64_unboxed"
  [@@noalloc] [@@no_effects] [@@no_coeffects] [@@builtin]

external select_int32
  :  bool
  -> (int32[@unboxed])
  -> (int32[@unboxed])
  -> (int32[@unboxed])
  = "caml_csel_value" "caml_csel_int32_unboxed"
  [@@noalloc] [@@no_effects] [@@no_coeffects] [@@builtin]

external select_nativeint
  :  bool
  -> (nativeint[@unboxed])
  -> (nativeint[@unboxed])
  -> (nativeint[@unboxed])
  = "caml_csel_value" "caml_csel_nativeint_unboxed"
  [@@noalloc] [@@no_effects] [@@no_coeffects] [@@builtin]
