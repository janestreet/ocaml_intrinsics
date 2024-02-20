(* Prefetch any ocaml value *)

(* The same native C stubs can be used for value and unboxed native pointer
   inputs: the C type of the formal argument is value and
   intnat, respectively, and value is defined as intnat. *)

(* Prefetching hints are meant for highly-optimized code, the bytecode stubs do nothing,
   so they all call the same C stub [caml_prefetch_ignore]. *)

(* Prefetching primitives should not be annotated with [@@no_effects].
   Otherwise, the compiler can eliminate them, because they have no result. *)

external prefetch_write_high
  :  'a
  -> unit
  = "caml_prefetch_ignore" "caml_prefetch_write_high"
  [@@noalloc] [@@builtin]

external prefetch_write_moderate
  :  'a
  -> unit
  = "caml_prefetch_ignore" "caml_prefetch_write_moderate"
  [@@noalloc] [@@builtin]

external prefetch_write_low
  :  'a
  -> unit
  = "caml_prefetch_ignore" "caml_prefetch_write_low"
  [@@noalloc] [@@builtin]

external prefetch_write_none
  :  'a
  -> unit
  = "caml_prefetch_ignore" "caml_prefetch_write_none"
  [@@noalloc] [@@builtin]

external prefetch_read_none
  :  'a
  -> unit
  = "caml_prefetch_ignore" "caml_prefetch_read_none"
  [@@noalloc] [@@builtin]

external prefetch_read_low : 'a -> unit = "caml_prefetch_ignore" "caml_prefetch_read_low"
  [@@noalloc] [@@builtin]

external prefetch_read_moderate
  :  'a
  -> unit
  = "caml_prefetch_ignore" "caml_prefetch_read_moderate"
  [@@noalloc] [@@builtin]

external prefetch_read_high
  :  'a
  -> unit
  = "caml_prefetch_ignore" "caml_prefetch_read_high"
  [@@noalloc] [@@builtin]

(* Prefetch at the given byte offset from an OCaml value. *)

external prefetch_write_high_val_offset
  :  'a
  -> byte_offset:(int[@untagged])
  -> unit
  = "caml_prefetch_ignore2" "caml_prefetch_write_high_val_offset_untagged"
  [@@noalloc] [@@builtin]

external prefetch_write_moderate_val_offset
  :  'a
  -> byte_offset:(int[@untagged])
  -> unit
  = "caml_prefetch_ignore2" "caml_prefetch_write_moderate_val_offset_untagged"
  [@@noalloc] [@@builtin]

external prefetch_write_low_val_offset
  :  'a
  -> byte_offset:(int[@untagged])
  -> unit
  = "caml_prefetch_ignore2" "caml_prefetch_write_low_val_offset_untagged"
  [@@noalloc] [@@builtin]

external prefetch_write_none_val_offset
  :  'a
  -> byte_offset:(int[@untagged])
  -> unit
  = "caml_prefetch_ignore2" "caml_prefetch_write_none_val_offset_untagged"
  [@@noalloc] [@@builtin]

external prefetch_read_none_val_offset
  :  'a
  -> byte_offset:(int[@untagged])
  -> unit
  = "caml_prefetch_ignore2" "caml_prefetch_read_none_val_offset_untagged"
  [@@noalloc] [@@builtin]

external prefetch_read_low_val_offset
  :  'a
  -> byte_offset:(int[@untagged])
  -> unit
  = "caml_prefetch_ignore2" "caml_prefetch_read_low_val_offset_untagged"
  [@@noalloc] [@@builtin]

external prefetch_read_moderate_val_offset
  :  'a
  -> byte_offset:(int[@untagged])
  -> unit
  = "caml_prefetch_ignore2" "caml_prefetch_read_moderate_val_offset_untagged"
  [@@noalloc] [@@builtin]

external prefetch_read_high_val_offset
  :  'a
  -> byte_offset:(int[@untagged])
  -> unit
  = "caml_prefetch_ignore2" "caml_prefetch_read_high_val_offset_untagged"
  [@@noalloc] [@@builtin]

(* Native_pointer *)

external prefetch_write_high_native_pointer
  :  (Native_pointer.t[@unboxed])
  -> unit
  = "caml_prefetch_ignore" "caml_prefetch_write_high"
  [@@noalloc] [@@builtin]

external prefetch_write_moderate_native_pointer
  :  (Native_pointer.t[@unboxed])
  -> unit
  = "caml_prefetch_ignore" "caml_prefetch_write_moderate"
  [@@noalloc] [@@builtin]

external prefetch_write_low_native_pointer
  :  (Native_pointer.t[@unboxed])
  -> unit
  = "caml_prefetch_ignore" "caml_prefetch_write_low"
  [@@noalloc] [@@builtin]

external prefetch_write_none_native_pointer
  :  (Native_pointer.t[@unboxed])
  -> unit
  = "caml_prefetch_ignore" "caml_prefetch_write_none"
  [@@noalloc] [@@builtin]

external prefetch_read_none_native_pointer
  :  (Native_pointer.t[@unboxed])
  -> unit
  = "caml_prefetch_ignore" "caml_prefetch_read_none"
  [@@noalloc] [@@builtin]

external prefetch_read_low_native_pointer
  :  (Native_pointer.t[@unboxed])
  -> unit
  = "caml_prefetch_ignore" "caml_prefetch_read_low"
  [@@noalloc] [@@builtin]

external prefetch_read_moderate_native_pointer
  :  (Native_pointer.t[@unboxed])
  -> unit
  = "caml_prefetch_ignore" "caml_prefetch_read_moderate"
  [@@noalloc] [@@builtin]

external prefetch_read_high_native_pointer
  :  (Native_pointer.t[@unboxed])
  -> unit
  = "caml_prefetch_ignore" "caml_prefetch_read_high"
  [@@noalloc] [@@builtin]

(* Ext_pointer *)

external prefetch_write_high_ext_pointer
  :  Ext_pointer.t
  -> unit
  = "caml_prefetch_ignore" "caml_prefetch_write_high_ext_pointer"
  [@@noalloc] [@@builtin]

external prefetch_write_moderate_ext_pointer
  :  Ext_pointer.t
  -> unit
  = "caml_prefetch_ignore" "caml_prefetch_write_moderate_ext_pointer"
  [@@noalloc] [@@builtin]

external prefetch_write_low_ext_pointer
  :  Ext_pointer.t
  -> unit
  = "caml_prefetch_ignore" "caml_prefetch_write_low_ext_pointer"
  [@@noalloc] [@@builtin]

external prefetch_write_none_ext_pointer
  :  Ext_pointer.t
  -> unit
  = "caml_prefetch_ignore" "caml_prefetch_write_none_ext_pointer"
  [@@noalloc] [@@builtin]

external prefetch_read_none_ext_pointer
  :  Ext_pointer.t
  -> unit
  = "caml_prefetch_ignore" "caml_prefetch_read_none_ext_pointer"
  [@@noalloc] [@@builtin]

external prefetch_read_low_ext_pointer
  :  Ext_pointer.t
  -> unit
  = "caml_prefetch_ignore" "caml_prefetch_read_low_ext_pointer"
  [@@noalloc] [@@builtin]

external prefetch_read_moderate_ext_pointer
  :  Ext_pointer.t
  -> unit
  = "caml_prefetch_ignore" "caml_prefetch_read_moderate_ext_pointer"
  [@@noalloc] [@@builtin]

external prefetch_read_high_ext_pointer
  :  Ext_pointer.t
  -> unit
  = "caml_prefetch_ignore" "caml_prefetch_read_high_ext_pointer"
  [@@noalloc] [@@builtin]

(* Bigstring *)

external prefetch_write_high_bigstring
  :  Bigstring_intf.t
  -> (int[@untagged])
  -> unit
  = "caml_prefetch_ignore2" "caml_prefetch_write_high_bigstring_untagged"
  [@@noalloc] [@@builtin]

external prefetch_write_moderate_bigstring
  :  Bigstring_intf.t
  -> (int[@untagged])
  -> unit
  = "caml_prefetch_ignore2" "caml_prefetch_write_moderate_bigstring_untagged"
  [@@noalloc] [@@builtin]

external prefetch_write_low_bigstring
  :  Bigstring_intf.t
  -> (int[@untagged])
  -> unit
  = "caml_prefetch_ignore2" "caml_prefetch_write_low_bigstring_untagged"
  [@@noalloc] [@@builtin]

external prefetch_write_none_bigstring
  :  Bigstring_intf.t
  -> (int[@untagged])
  -> unit
  = "caml_prefetch_ignore2" "caml_prefetch_write_none_bigstring_untagged"
  [@@noalloc] [@@builtin]

external prefetch_read_none_bigstring
  :  Bigstring_intf.t
  -> (int[@untagged])
  -> unit
  = "caml_prefetch_ignore2" "caml_prefetch_read_none_bigstring_untagged"
  [@@noalloc] [@@builtin]

external prefetch_read_low_bigstring
  :  Bigstring_intf.t
  -> (int[@untagged])
  -> unit
  = "caml_prefetch_ignore2" "caml_prefetch_read_low_bigstring_untagged"
  [@@noalloc] [@@builtin]

external prefetch_read_moderate_bigstring
  :  Bigstring_intf.t
  -> (int[@untagged])
  -> unit
  = "caml_prefetch_ignore2" "caml_prefetch_read_moderate_bigstring_untagged"
  [@@noalloc] [@@builtin]

external prefetch_read_high_bigstring
  :  Bigstring_intf.t
  -> (int[@untagged])
  -> unit
  = "caml_prefetch_ignore2" "caml_prefetch_read_high_bigstring_untagged"
  [@@noalloc] [@@builtin]
