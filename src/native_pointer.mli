(**  [Native_pointer] uses Nativeint to hold a pointer to a memory block allocated
     outside the OCaml heap. The pointer is not required to be aligned. *)
type t = private nativeint

(**
   [ext_pointer_as_native_pointer p] takes an int [p] that encodes
   a pointer to a memory block outside of the OCaml heap,
   decodes [p] by clearing the least significant bit of [p],
   and boxes the result as [nativeint].
   Unlike untagging, decoding [p] does not shift the bits of [p].
*)
external ext_pointer_as_native_pointer
  :  int
  -> (t[@unboxed])
  = "caml_ext_pointer_as_native_pointer_bytecode" "caml_ext_pointer_as_native_pointer"
  [@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]

(**
   Reinterpret any 'a as a native pointer. The unboxed result will have the same bit
   representation as the input value.
   If 'a is a pointer to outside the OCaml heap, the result may be used
   with load or store operations. *)
external unsafe_of_value
  :  'a
  -> (t[@unboxed])
  = "caml_native_pointer_of_value_bytecode" "caml_native_pointer_of_value"
  [@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]

(**
   Reinterpret the unboxed contents of a native pointer as a value of any type. The
   result will have the same bit representation as the unboxed input pointer.
   The result must be a valid OCaml value: either an immediate or a pointer to an
   address with a valid OCaml header. *)
external unsafe_to_value
  :  (t[@unboxed])
  -> 'a
  = "caml_native_pointer_to_value_bytecode" "caml_native_pointer_to_value"
  [@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]

(** [load_untagged_int t] reads untagged int pointed to by [t] and returns
    the corresponding tagged int. This should only be used to read a value
    written by [store_untagged_int]. Otherwise, if the value has most significant
    bit set, it will be lost by tagging. To avoid it,
    use [load_unboxed_nativeint] and check before converting to int
    (should not allocate). Their native code C stub is the same. *)
external load_untagged_int
  :  (t[@unboxed])
  -> (int[@untagged])
  = "caml_native_pointer_load_untagged_int_bytecode"
    "caml_native_pointer_load_unboxed_nativeint"
  [@@noalloc] [@@builtin] [@@no_effects]

(** [store_untagged_int t d] untags [d] and stores the result to the memory pointed to by
    [t]. *)
external store_untagged_int
  :  (t[@unboxed])
  -> (int[@untagged])
  -> unit
  = "caml_native_pointer_store_untagged_int_bytecode"
    "caml_native_pointer_store_unboxed_nativeint"
  [@@noalloc] [@@builtin] [@@no_coeffects]

(** [load_unboxed_nativeint t] reads unboxed nativeint pointed to by [t] and returns
    the corresponding (boxed) nativeint allocated on the OCaml heap. *)
external load_unboxed_nativeint
  :  t
  -> nativeint
  = "caml_native_pointer_load_unboxed_nativeint_bytecode"
    "caml_native_pointer_load_unboxed_nativeint"
  [@@unboxed] [@@noalloc] [@@builtin] [@@no_effects]

(** [store_unboxed_nativeint t d] stores the unboxed nativeint to the memory pointed to by
    [t]. *)
external store_unboxed_nativeint
  :  (t[@unboxed])
  -> (nativeint[@unboxed])
  -> unit
  = "caml_native_pointer_store_unboxed_nativeint_bytecode"
    "caml_native_pointer_store_unboxed_nativeint"
  [@@noalloc] [@@builtin] [@@no_coeffects]

(** [load_unboxed_int64 t] reads unboxed int64 pointed to by [t] and returns
    the corresponding (boxed) int64 allocated on the OCaml heap. *)
external load_unboxed_int64
  :  t
  -> int64
  = "caml_native_pointer_load_unboxed_int64_bytecode"
    "caml_native_pointer_load_unboxed_int64"
  [@@unboxed] [@@noalloc] [@@builtin] [@@no_effects]

(** [store_unboxed_int64 t d] stores the unboxed int64 to the memory pointed to by [t]. *)
external store_unboxed_int64
  :  (t[@unboxed])
  -> (int64[@unboxed])
  -> unit
  = "caml_native_pointer_store_unboxed_int64_bytecode"
    "caml_native_pointer_store_unboxed_int64"
  [@@noalloc] [@@builtin] [@@no_coeffects]

(** [load_unboxed_int32 t] reads unboxed int32 pointed to by [t] and returns
    the corresponding (boxed) int32 allocated on the OCaml heap. *)
external load_unboxed_int32
  :  t
  -> int32
  = "caml_native_pointer_load_unboxed_int32_bytecode"
    "caml_native_pointer_load_unboxed_int32"
  [@@unboxed] [@@noalloc] [@@builtin] [@@no_effects]

(** [store_unboxed_int32 t d] stores the unboxed int32 to the memory pointed to by [t]. *)
external store_unboxed_int32
  :  (t[@unboxed])
  -> (int32[@unboxed])
  -> unit
  = "caml_native_pointer_store_unboxed_int32_bytecode"
    "caml_native_pointer_store_unboxed_int32"
  [@@noalloc] [@@builtin] [@@no_coeffects]

(** For float operations, the pointer must be aligned at least to the native integer
    machine width (meaning on 32-bit platforms, a 32-bit-aligned pointer is acceptable
    even though the width of the float is 64 bits). *)

(** [load_unboxed_float t] reads the unboxed float pointed to by [t].  (If the result is
    not directly passed to another operation expecting an unboxed float, then it will
    be boxed.) *)
external load_unboxed_float
  :  t
  -> float
  = "caml_native_pointer_load_unboxed_float_bytecode"
    "caml_native_pointer_load_unboxed_float"
  [@@unboxed] [@@noalloc] [@@builtin] [@@no_effects]

(** [store_unboxed_float t d] stores the unboxed float to the memory pointed to by [t]. *)
external store_unboxed_float
  :  (t[@unboxed])
  -> (float[@unboxed])
  -> unit
  = "caml_native_pointer_store_unboxed_float_bytecode"
    "caml_native_pointer_store_unboxed_float"
  [@@noalloc] [@@builtin] [@@no_coeffects]

(** Pointer arithmetic and comparisons *)

(** [difference_in_bytes start end] subtracts start from end, assuming that both point
    to the same block of memory (such as an array). The result is the number of bytes,
    not a pointer.  *)
val difference_in_bytes : t -> t -> nativeint

(** Adds [n] bytes to a pointer, resulting in another pointer. *)
val advance : t -> bytes:nativeint -> t

(** Equal operator is defined for any two pointers. Inequality operators are only defined
    when both pointers point to the same block of memory (such as an array). *)
val ( = ) : t -> t -> bool

val ( <> ) : t -> t -> bool
val ( < ) : t -> t -> bool
val ( > ) : t -> t -> bool
val ( <= ) : t -> t -> bool
val ( >= ) : t -> t -> bool

(** Load and store immediate values. Intended for use with [Int] and [Bool]. *)
module type Immediate_intf = sig
  module V : sig
    type t [@@immediate64]
  end

  external unsafe_load_immediate
    :  (t[@unboxed])
    -> V.t
    = "caml_native_pointer_load_immediate_bytecode" "caml_native_pointer_load_immediate"
    [@@noalloc] [@@builtin] [@@no_effects]

  external store_immediate
    :  (t[@unboxed])
    -> V.t
    -> unit
    = "caml_native_pointer_store_immediate_bytecode" "caml_native_pointer_store_immediate"
    [@@noalloc] [@@builtin] [@@no_coeffects]
end

module Int : Immediate_intf with type V.t = Stdlib.Int.t
module Bool : Immediate_intf with type V.t = Stdlib.Bool.t

module Expert : sig
  (** These are *not* equivalent to unsafe_of/to_value: they convert between a boxed
      nativeint and a boxed native pointer (as in Obj.magic), which is always legal
      because they have the same representation. *)
  external of_nativeint : nativeint -> t = "%identity"

  external to_nativeint : t -> nativeint = "%identity"
end
