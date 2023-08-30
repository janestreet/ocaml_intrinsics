(** Untagged int *)

external native_pointer_fetch_and_add_int
  :  (Native_pointer.t[@unboxed])
  -> (int[@untagged])
  -> (int[@untagged])
  = "caml_native_pointer_fetch_and_add_int_bytecode"
    "caml_native_pointer_fetch_and_add_int_untagged"
  [@@noalloc] [@@builtin]

external native_pointer_fetch_and_sub_int
  :  (Native_pointer.t[@unboxed])
  -> (int[@untagged])
  -> (int[@untagged])
  = "caml_native_pointer_fetch_and_sub_int_bytecode"
    "caml_native_pointer_fetch_and_sub_int_untagged"
  [@@noalloc] [@@builtin]

external native_pointer_compare_and_swap_int
  :  (Native_pointer.t[@unboxed])
  -> compare_with:(int[@untagged])
  -> set_to:(int[@untagged])
  -> bool
  = "caml_native_pointer_compare_and_swap_int_bytecode"
    "caml_native_pointer_compare_and_swap_int_untagged"
  [@@noalloc] [@@builtin]

external ext_pointer_fetch_and_add_int
  :  Ext_pointer.t
  -> (int[@untagged])
  -> (int[@untagged])
  = "caml_ext_pointer_fetch_and_add_int_bytecode"
    "caml_ext_pointer_fetch_and_add_int_untagged"
  [@@noalloc] [@@builtin]

external ext_pointer_fetch_and_sub_int
  :  Ext_pointer.t
  -> (int[@untagged])
  -> (int[@untagged])
  = "caml_ext_pointer_fetch_and_sub_int_bytecode"
    "caml_ext_pointer_fetch_and_sub_int_untagged"
  [@@noalloc] [@@builtin]

external ext_pointer_compare_and_swap_int
  :  Ext_pointer.t
  -> compare_with:(int[@untagged])
  -> set_to:(int[@untagged])
  -> bool
  = "caml_ext_pointer_compare_and_swap_int_bytecode"
    "caml_ext_pointer_compare_and_swap_int_untagged"
  [@@noalloc] [@@builtin]

external bigstring_fetch_and_add_int
  :  Bigstring_intf.t
  -> pos:(int[@untagged])
  -> (int[@untagged])
  -> (int[@untagged])
  = "caml_bigstring_fetch_and_add_int_bytecode"
    "caml_bigstring_fetch_and_add_int_untagged"
  [@@noalloc] [@@builtin]

external bigstring_fetch_and_sub_int
  :  Bigstring_intf.t
  -> pos:(int[@untagged])
  -> (int[@untagged])
  -> (int[@untagged])
  = "caml_bigstring_fetch_and_sub_int_bytecode"
    "caml_bigstring_fetch_and_sub_int_untagged"
  [@@noalloc] [@@builtin]

external bigstring_compare_and_swap_int
  :  Bigstring_intf.t
  -> pos:(int[@untagged])
  -> compare_with:(int[@untagged])
  -> set_to:(int[@untagged])
  -> bool
  = "caml_bigstring_compare_and_swap_int_bytecode"
    "caml_bigstring_compare_and_swap_int_untagged"
  [@@noalloc] [@@builtin]

(** Unboxed int64 *)

external native_pointer_fetch_and_add_int64
  :  (Native_pointer.t[@unboxed])
  -> (int64[@unboxed])
  -> (int64[@unboxed])
  = "caml_native_pointer_fetch_and_add_int64_bytecode"
    "caml_native_pointer_fetch_and_add_int64_unboxed"
  [@@noalloc] [@@builtin]

external native_pointer_fetch_and_sub_int64
  :  (Native_pointer.t[@unboxed])
  -> (int64[@unboxed])
  -> (int64[@unboxed])
  = "caml_native_pointer_fetch_and_sub_int64_bytecode"
    "caml_native_pointer_fetch_and_sub_int64_unboxed"
  [@@noalloc] [@@builtin]

external native_pointer_compare_and_swap_int64
  :  (Native_pointer.t[@unboxed])
  -> compare_with:(int64[@unboxed])
  -> set_to:(int64[@unboxed])
  -> bool
  = "caml_native_pointer_compare_and_swap_int64_bytecode"
    "caml_native_pointer_compare_and_swap_int64_unboxed"
  [@@noalloc] [@@builtin]

external ext_pointer_fetch_and_add_int64
  :  Ext_pointer.t
  -> (int64[@unboxed])
  -> (int64[@unboxed])
  = "caml_ext_pointer_fetch_and_add_int64_bytecode"
    "caml_ext_pointer_fetch_and_add_int64_unboxed"
  [@@noalloc] [@@builtin]

external ext_pointer_fetch_and_sub_int64
  :  Ext_pointer.t
  -> (int64[@unboxed])
  -> (int64[@unboxed])
  = "caml_ext_pointer_fetch_and_sub_int64_bytecode"
    "caml_ext_pointer_fetch_and_sub_int64_unboxed"
  [@@noalloc] [@@builtin]

external ext_pointer_compare_and_swap_int64
  :  Ext_pointer.t
  -> compare_with:(int64[@unboxed])
  -> set_to:(int64[@unboxed])
  -> bool
  = "caml_ext_pointer_compare_and_swap_int64_bytecode"
    "caml_ext_pointer_compare_and_swap_int64_unboxed"
  [@@noalloc] [@@builtin]

external bigstring_fetch_and_add_int64
  :  Bigstring_intf.t
  -> pos:(int[@untagged])
  -> (int64[@unboxed])
  -> (int64[@unboxed])
  = "caml_bigstring_fetch_and_add_int64_bytecode"
    "caml_bigstring_fetch_and_add_int64_unboxed"
  [@@noalloc] [@@builtin]

external bigstring_fetch_and_sub_int64
  :  Bigstring_intf.t
  -> pos:(int[@untagged])
  -> (int64[@unboxed])
  -> (int64[@unboxed])
  = "caml_bigstring_fetch_and_sub_int64_bytecode"
    "caml_bigstring_fetch_and_sub_int64_unboxed"
  [@@noalloc] [@@builtin]

external bigstring_compare_and_swap_int64
  :  Bigstring_intf.t
  -> pos:(int[@untagged])
  -> compare_with:(int64[@unboxed])
  -> set_to:(int64[@unboxed])
  -> bool
  = "caml_bigstring_compare_and_swap_int64_bytecode"
    "caml_bigstring_compare_and_swap_int64_unboxed"
  [@@noalloc] [@@builtin]

(** Unboxed int32 *)

external native_pointer_fetch_and_add_int32
  :  (Native_pointer.t[@unboxed])
  -> (int32[@unboxed])
  -> (int32[@unboxed])
  = "caml_native_pointer_fetch_and_add_int32_bytecode"
    "caml_native_pointer_fetch_and_add_int32_unboxed"
  [@@noalloc] [@@builtin]

external native_pointer_fetch_and_sub_int32
  :  (Native_pointer.t[@unboxed])
  -> (int32[@unboxed])
  -> (int32[@unboxed])
  = "caml_native_pointer_fetch_and_sub_int32_bytecode"
    "caml_native_pointer_fetch_and_sub_int32_unboxed"
  [@@noalloc] [@@builtin]

external native_pointer_compare_and_swap_int32
  :  (Native_pointer.t[@unboxed])
  -> compare_with:(int32[@unboxed])
  -> set_to:(int32[@unboxed])
  -> bool
  = "caml_native_pointer_compare_and_swap_int32_bytecode"
    "caml_native_pointer_compare_and_swap_int32_unboxed"
  [@@noalloc] [@@builtin]

external ext_pointer_fetch_and_add_int32
  :  Ext_pointer.t
  -> (int32[@unboxed])
  -> (int32[@unboxed])
  = "caml_ext_pointer_fetch_and_add_int32_bytecode"
    "caml_ext_pointer_fetch_and_add_int32_unboxed"
  [@@noalloc] [@@builtin]

external ext_pointer_fetch_and_sub_int32
  :  Ext_pointer.t
  -> (int32[@unboxed])
  -> (int32[@unboxed])
  = "caml_ext_pointer_fetch_and_sub_int32_bytecode"
    "caml_ext_pointer_fetch_and_sub_int32_unboxed"
  [@@noalloc] [@@builtin]

external ext_pointer_compare_and_swap_int32
  :  Ext_pointer.t
  -> compare_with:(int32[@unboxed])
  -> set_to:(int32[@unboxed])
  -> bool
  = "caml_ext_pointer_compare_and_swap_int32_bytecode"
    "caml_ext_pointer_compare_and_swap_int32_unboxed"
  [@@noalloc] [@@builtin]

external bigstring_fetch_and_add_int32
  :  Bigstring_intf.t
  -> pos:(int[@untagged])
  -> (int32[@unboxed])
  -> (int32[@unboxed])
  = "caml_bigstring_fetch_and_add_int32_bytecode"
    "caml_bigstring_fetch_and_add_int32_unboxed"
  [@@noalloc] [@@builtin]

external bigstring_fetch_and_sub_int32
  :  Bigstring_intf.t
  -> pos:(int[@untagged])
  -> (int32[@unboxed])
  -> (int32[@unboxed])
  = "caml_bigstring_fetch_and_sub_int32_bytecode"
    "caml_bigstring_fetch_and_sub_int32_unboxed"
  [@@noalloc] [@@builtin]

external bigstring_compare_and_swap_int32
  :  Bigstring_intf.t
  -> pos:(int[@untagged])
  -> compare_with:(int32[@unboxed])
  -> set_to:(int32[@unboxed])
  -> bool
  = "caml_bigstring_compare_and_swap_int32_bytecode"
    "caml_bigstring_compare_and_swap_int32_unboxed"
  [@@noalloc] [@@builtin]

(** Unboxed nativeint *)

external native_pointer_fetch_and_add_nativeint
  :  (Native_pointer.t[@unboxed])
  -> (nativeint[@unboxed])
  -> (nativeint[@unboxed])
  = "caml_native_pointer_fetch_and_add_nativeint_bytecode"
    "caml_native_pointer_fetch_and_add_nativeint_unboxed"
  [@@noalloc] [@@builtin]

external native_pointer_fetch_and_sub_nativeint
  :  (Native_pointer.t[@unboxed])
  -> (nativeint[@unboxed])
  -> (nativeint[@unboxed])
  = "caml_native_pointer_fetch_and_sub_nativeint_bytecode"
    "caml_native_pointer_fetch_and_sub_nativeint_unboxed"
  [@@noalloc] [@@builtin]

external native_pointer_compare_and_swap_nativeint
  :  (Native_pointer.t[@unboxed])
  -> compare_with:(nativeint[@unboxed])
  -> set_to:(nativeint[@unboxed])
  -> bool
  = "caml_native_pointer_compare_and_swap_nativeint_bytecode"
    "caml_native_pointer_compare_and_swap_nativeint_unboxed"
  [@@noalloc] [@@builtin]

external ext_pointer_fetch_and_add_nativeint
  :  Ext_pointer.t
  -> (nativeint[@unboxed])
  -> (nativeint[@unboxed])
  = "caml_ext_pointer_fetch_and_add_nativeint_bytecode"
    "caml_ext_pointer_fetch_and_add_nativeint_unboxed"
  [@@noalloc] [@@builtin]

external ext_pointer_fetch_and_sub_nativeint
  :  Ext_pointer.t
  -> (nativeint[@unboxed])
  -> (nativeint[@unboxed])
  = "caml_ext_pointer_fetch_and_sub_nativeint_bytecode"
    "caml_ext_pointer_fetch_and_sub_nativeint_unboxed"
  [@@noalloc] [@@builtin]

external ext_pointer_compare_and_swap_nativeint
  :  Ext_pointer.t
  -> compare_with:(nativeint[@unboxed])
  -> set_to:(nativeint[@unboxed])
  -> bool
  = "caml_ext_pointer_compare_and_swap_nativeint_bytecode"
    "caml_ext_pointer_compare_and_swap_nativeint_unboxed"
  [@@noalloc] [@@builtin]

external bigstring_fetch_and_add_nativeint
  :  Bigstring_intf.t
  -> pos:(int[@untagged])
  -> (nativeint[@unboxed])
  -> (nativeint[@unboxed])
  = "caml_bigstring_fetch_and_add_nativeint_bytecode"
    "caml_bigstring_fetch_and_add_nativeint_unboxed"
  [@@noalloc] [@@builtin]

external bigstring_fetch_and_sub_nativeint
  :  Bigstring_intf.t
  -> pos:(int[@untagged])
  -> (nativeint[@unboxed])
  -> (nativeint[@unboxed])
  = "caml_bigstring_fetch_and_sub_nativeint_bytecode"
    "caml_bigstring_fetch_and_sub_nativeint_unboxed"
  [@@noalloc] [@@builtin]

external bigstring_compare_and_swap_nativeint
  :  Bigstring_intf.t
  -> pos:(int[@untagged])
  -> compare_with:(nativeint[@unboxed])
  -> set_to:(nativeint[@unboxed])
  -> bool
  = "caml_bigstring_compare_and_swap_nativeint_bytecode"
    "caml_bigstring_compare_and_swap_nativeint_unboxed"
  [@@noalloc] [@@builtin]
