type temporal_locality =
  | None
  | Low
  | Moderate
  | High

type operation =
  | Read
  | Write

external pause : unit -> unit = "caml_pause_hint" [@@noalloc]

external cldemote
  :  (Native_pointer.t[@unboxed])
  -> unit
  = "caml_cldemote_ignore" "caml_cldemote"
[@@noalloc] [@@builtin amd64]

(* Prefetching hints are meant for highly-optimized code, the bytecode stubs do nothing,
   so they all call the same C stub [caml_prefetch_ignore].

   Prefetching primitives should not be annotated with [@@no_effects]. Otherwise, the
   compiler can eliminate them, because they have no result. *)
module Value = struct
  external prefetch_write_high
    : 'a.
    'a -> unit
    = "caml_prefetch_ignore" "caml_prefetch_write_high"
  [@@noalloc] [@@builtin amd64]

  external prefetch_write_moderate
    : 'a.
    'a -> unit
    = "caml_prefetch_ignore" "caml_prefetch_write_moderate"
  [@@noalloc] [@@builtin amd64]

  external prefetch_write_low
    : 'a.
    'a -> unit
    = "caml_prefetch_ignore" "caml_prefetch_write_low"
  [@@noalloc] [@@builtin amd64]

  external prefetch_write_none
    : 'a.
    'a -> unit
    = "caml_prefetch_ignore" "caml_prefetch_write_none"
  [@@noalloc] [@@builtin amd64]

  external prefetch_read_none
    : 'a.
    'a -> unit
    = "caml_prefetch_ignore" "caml_prefetch_read_none"
  [@@noalloc] [@@builtin amd64]

  external prefetch_read_low
    : 'a.
    'a -> unit
    = "caml_prefetch_ignore" "caml_prefetch_read_low"
  [@@noalloc] [@@builtin amd64]

  external prefetch_read_moderate
    : 'a.
    'a -> unit
    = "caml_prefetch_ignore" "caml_prefetch_read_moderate"
  [@@noalloc] [@@builtin amd64]

  external prefetch_read_high
    : 'a.
    'a -> unit
    = "caml_prefetch_ignore" "caml_prefetch_read_high"
  [@@noalloc] [@@builtin amd64]

  external prefetch_write_high_val_offset
    : 'a.
    'a -> byte_offset:(int[@untagged]) -> unit
    = "caml_prefetch_ignore2" "caml_prefetch_write_high_val_offset_untagged"
  [@@noalloc] [@@builtin amd64]

  external prefetch_write_moderate_val_offset
    : 'a.
    'a -> byte_offset:(int[@untagged]) -> unit
    = "caml_prefetch_ignore2" "caml_prefetch_write_moderate_val_offset_untagged"
  [@@noalloc] [@@builtin amd64]

  external prefetch_write_low_val_offset
    : 'a.
    'a -> byte_offset:(int[@untagged]) -> unit
    = "caml_prefetch_ignore2" "caml_prefetch_write_low_val_offset_untagged"
  [@@noalloc] [@@builtin amd64]

  external prefetch_write_none_val_offset
    : 'a.
    'a -> byte_offset:(int[@untagged]) -> unit
    = "caml_prefetch_ignore2" "caml_prefetch_write_none_val_offset_untagged"
  [@@noalloc] [@@builtin amd64]

  external prefetch_read_none_val_offset
    : 'a.
    'a -> byte_offset:(int[@untagged]) -> unit
    = "caml_prefetch_ignore2" "caml_prefetch_read_none_val_offset_untagged"
  [@@noalloc] [@@builtin amd64]

  external prefetch_read_low_val_offset
    : 'a.
    'a -> byte_offset:(int[@untagged]) -> unit
    = "caml_prefetch_ignore2" "caml_prefetch_read_low_val_offset_untagged"
  [@@noalloc] [@@builtin amd64]

  external prefetch_read_moderate_val_offset
    : 'a.
    'a -> byte_offset:(int[@untagged]) -> unit
    = "caml_prefetch_ignore2" "caml_prefetch_read_moderate_val_offset_untagged"
  [@@noalloc] [@@builtin amd64]

  external prefetch_read_high_val_offset
    : 'a.
    'a -> byte_offset:(int[@untagged]) -> unit
    = "caml_prefetch_ignore2" "caml_prefetch_read_high_val_offset_untagged"
  [@@noalloc] [@@builtin amd64]
end

(* The same native C stubs can be used for value and unboxed native pointer inputs: the C
   type of the formal argument is value and intnat, respectively, and value is defined as
   intnat. *)
module Native_pointer = struct
  external prefetch_write_high
    :  (Native_pointer.t[@unboxed])
    -> unit
    = "caml_prefetch_ignore" "caml_prefetch_write_high"
  [@@noalloc] [@@builtin amd64]

  external prefetch_write_moderate
    :  (Native_pointer.t[@unboxed])
    -> unit
    = "caml_prefetch_ignore" "caml_prefetch_write_moderate"
  [@@noalloc] [@@builtin amd64]

  external prefetch_write_low
    :  (Native_pointer.t[@unboxed])
    -> unit
    = "caml_prefetch_ignore" "caml_prefetch_write_low"
  [@@noalloc] [@@builtin amd64]

  external prefetch_write_none
    :  (Native_pointer.t[@unboxed])
    -> unit
    = "caml_prefetch_ignore" "caml_prefetch_write_none"
  [@@noalloc] [@@builtin amd64]

  external prefetch_read_none
    :  (Native_pointer.t[@unboxed])
    -> unit
    = "caml_prefetch_ignore" "caml_prefetch_read_none"
  [@@noalloc] [@@builtin amd64]

  external prefetch_read_low
    :  (Native_pointer.t[@unboxed])
    -> unit
    = "caml_prefetch_ignore" "caml_prefetch_read_low"
  [@@noalloc] [@@builtin amd64]

  external prefetch_read_moderate
    :  (Native_pointer.t[@unboxed])
    -> unit
    = "caml_prefetch_ignore" "caml_prefetch_read_moderate"
  [@@noalloc] [@@builtin amd64]

  external prefetch_read_high
    :  (Native_pointer.t[@unboxed])
    -> unit
    = "caml_prefetch_ignore" "caml_prefetch_read_high"
  [@@noalloc] [@@builtin amd64]
end

module Ext_pointer = struct
  external prefetch_write_high
    :  Ext_pointer.t
    -> unit
    = "caml_prefetch_ignore" "caml_prefetch_write_high_ext_pointer"
  [@@noalloc] [@@builtin amd64]

  external prefetch_write_moderate
    :  Ext_pointer.t
    -> unit
    = "caml_prefetch_ignore" "caml_prefetch_write_moderate_ext_pointer"
  [@@noalloc] [@@builtin amd64]

  external prefetch_write_low
    :  Ext_pointer.t
    -> unit
    = "caml_prefetch_ignore" "caml_prefetch_write_low_ext_pointer"
  [@@noalloc] [@@builtin amd64]

  external prefetch_write_none
    :  Ext_pointer.t
    -> unit
    = "caml_prefetch_ignore" "caml_prefetch_write_none_ext_pointer"
  [@@noalloc] [@@builtin amd64]

  external prefetch_read_none
    :  Ext_pointer.t
    -> unit
    = "caml_prefetch_ignore" "caml_prefetch_read_none_ext_pointer"
  [@@noalloc] [@@builtin amd64]

  external prefetch_read_low
    :  Ext_pointer.t
    -> unit
    = "caml_prefetch_ignore" "caml_prefetch_read_low_ext_pointer"
  [@@noalloc] [@@builtin amd64]

  external prefetch_read_moderate
    :  Ext_pointer.t
    -> unit
    = "caml_prefetch_ignore" "caml_prefetch_read_moderate_ext_pointer"
  [@@noalloc] [@@builtin amd64]

  external prefetch_read_high
    :  Ext_pointer.t
    -> unit
    = "caml_prefetch_ignore" "caml_prefetch_read_high_ext_pointer"
  [@@noalloc] [@@builtin amd64]
end

module Bigstring = struct
  external prefetch_write_high
    :  Bigstring_intf.t
    -> (int[@untagged])
    -> unit
    = "caml_prefetch_ignore2" "caml_prefetch_write_high_bigstring_untagged"
  [@@noalloc] [@@builtin amd64]

  external prefetch_write_moderate
    :  Bigstring_intf.t
    -> (int[@untagged])
    -> unit
    = "caml_prefetch_ignore2" "caml_prefetch_write_moderate_bigstring_untagged"
  [@@noalloc] [@@builtin amd64]

  external prefetch_write_low
    :  Bigstring_intf.t
    -> (int[@untagged])
    -> unit
    = "caml_prefetch_ignore2" "caml_prefetch_write_low_bigstring_untagged"
  [@@noalloc] [@@builtin amd64]

  external prefetch_write_none
    :  Bigstring_intf.t
    -> (int[@untagged])
    -> unit
    = "caml_prefetch_ignore2" "caml_prefetch_write_none_bigstring_untagged"
  [@@noalloc] [@@builtin amd64]

  external prefetch_read_none
    :  Bigstring_intf.t
    -> (int[@untagged])
    -> unit
    = "caml_prefetch_ignore2" "caml_prefetch_read_none_bigstring_untagged"
  [@@noalloc] [@@builtin amd64]

  external prefetch_read_low
    :  Bigstring_intf.t
    -> (int[@untagged])
    -> unit
    = "caml_prefetch_ignore2" "caml_prefetch_read_low_bigstring_untagged"
  [@@noalloc] [@@builtin amd64]

  external prefetch_read_moderate
    :  Bigstring_intf.t
    -> (int[@untagged])
    -> unit
    = "caml_prefetch_ignore2" "caml_prefetch_read_moderate_bigstring_untagged"
  [@@noalloc] [@@builtin amd64]

  external prefetch_read_high
    :  Bigstring_intf.t
    -> (int[@untagged])
    -> unit
    = "caml_prefetch_ignore2" "caml_prefetch_read_high_bigstring_untagged"
  [@@noalloc] [@@builtin amd64]
end

let value p ~operation ~temporal_locality =
  match operation, temporal_locality with
  | Write, High -> Value.prefetch_write_high p
  | Write, Moderate -> Value.prefetch_write_moderate p
  | Write, Low -> Value.prefetch_write_low p
  | Write, None -> Value.prefetch_write_none p
  | Read, None -> Value.prefetch_read_none p
  | Read, Low -> Value.prefetch_read_low p
  | Read, Moderate -> Value.prefetch_read_moderate p
  | Read, High -> Value.prefetch_read_high p
;;

let value_byte_offset p ~byte_offset ~operation ~temporal_locality =
  match operation, temporal_locality with
  | Write, High -> Value.prefetch_write_high_val_offset p ~byte_offset
  | Write, Moderate -> Value.prefetch_write_moderate_val_offset p ~byte_offset
  | Write, Low -> Value.prefetch_write_low_val_offset p ~byte_offset
  | Write, None -> Value.prefetch_write_none_val_offset p ~byte_offset
  | Read, None -> Value.prefetch_read_none_val_offset p ~byte_offset
  | Read, Low -> Value.prefetch_read_low_val_offset p ~byte_offset
  | Read, Moderate -> Value.prefetch_read_moderate_val_offset p ~byte_offset
  | Read, High -> Value.prefetch_read_high_val_offset p ~byte_offset
;;

let value_pos p ~pos ~operation ~temporal_locality =
  let byte_offset = Sys.word_size / 8 * pos in
  value_byte_offset p ~byte_offset ~operation ~temporal_locality
;;

let native_pointer p ~operation ~temporal_locality =
  match operation, temporal_locality with
  | Write, High -> Native_pointer.prefetch_write_high p
  | Write, Moderate -> Native_pointer.prefetch_write_moderate p
  | Write, Low -> Native_pointer.prefetch_write_low p
  | Write, None -> Native_pointer.prefetch_write_none p
  | Read, None -> Native_pointer.prefetch_read_none p
  | Read, Low -> Native_pointer.prefetch_read_low p
  | Read, Moderate -> Native_pointer.prefetch_read_moderate p
  | Read, High -> Native_pointer.prefetch_read_high p
;;

let ext_pointer p ~operation ~temporal_locality =
  match operation, temporal_locality with
  | Write, High -> Ext_pointer.prefetch_write_high p
  | Write, Moderate -> Ext_pointer.prefetch_write_moderate p
  | Write, Low -> Ext_pointer.prefetch_write_low p
  | Write, None -> Ext_pointer.prefetch_write_none p
  | Read, None -> Ext_pointer.prefetch_read_none p
  | Read, Low -> Ext_pointer.prefetch_read_low p
  | Read, Moderate -> Ext_pointer.prefetch_read_moderate p
  | Read, High -> Ext_pointer.prefetch_read_high p
;;

let bigstring bigstring ~pos ~operation ~temporal_locality =
  match operation, temporal_locality with
  | Write, High -> Bigstring.prefetch_write_high bigstring pos
  | Write, Moderate -> Bigstring.prefetch_write_moderate bigstring pos
  | Write, Low -> Bigstring.prefetch_write_low bigstring pos
  | Write, None -> Bigstring.prefetch_write_none bigstring pos
  | Read, None -> Bigstring.prefetch_read_none bigstring pos
  | Read, Low -> Bigstring.prefetch_read_low bigstring pos
  | Read, Moderate -> Bigstring.prefetch_read_moderate bigstring pos
  | Read, High -> Bigstring.prefetch_read_high bigstring pos
;;
