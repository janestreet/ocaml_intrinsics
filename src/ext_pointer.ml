(** [Ext_pointer] uses values of the OCaml type "int" to represent pointers to 2-byte
    aligned memory blocks allocated outside the OCaml heap.

    The least significant bit of the address of a 2-byte aligned memory block is 0. This
    bit is used by this library to represent the address as an OCaml int, which prevents
    the OCaml GC from following pointers to external memory.

    To encode an external pointer as int: set the least significant bit. To decode int
    into an external pointer: clear the least significant bit. Note that these encode and
    decode operations are not the same as tagging and untagging operations on OCaml
    representation of int, which involves shifting.

    [Ext_pointer] allows OCaml code to pass around and manipulate pointers to external
    memory blocks without using "naked pointers".

    Note that "unboxed" and "untagged" in function names refer to the memory
    representation of the values, not the types of arguments and results. For example,
    [store_unboxed_float] and [store_untagged_int] is for values that are stored in memory
    in their unboxed and untagged form, respectively. Additionally, [Unboxed] submodule
    indicates that the type of arguments or results is unboxed. This is not the same as
    [@unboxed] attribute, which is an implementation detail of the C stubs. *)

type t = private int

external create : int -> t = "%identity"

(** [offset_by_2n_bytes t n] represents an external pointer to address [t + 2*n]. *)
let offset_by_2n_bytes (t : t) n =
  (* Because the least significant bit of the pointer is implicitly zero, adding to the
     represented integer value adds twice as much to the represented pointer value.
     Argument is doubled to ensure that the resulting address is 2-byte aligned. Checking
     that the argument is even would result in worse code generation with a conditional
     branch. *)
  create ((t :> int) + n)
;;

module Immediate (V : sig
    type t : immediate
  end) =
struct
  (** [load_immediate t] assumes without checking that the value pointed to by [t] is
      immediate. *)
  external unsafe_load_immediate : t -> V.t = "caml_ext_pointer_load_immediate"
  [@@noalloc]

  (** [store_int t i] stores the immediate [i] to the memory address represented by [t]. *)
  external store_immediate : t -> V.t -> unit = "caml_ext_pointer_store_immediate"
  [@@noalloc]
end

module Int = Immediate (Stdlib.Int)
module Bool = Immediate (Stdlib.Bool)

(** [load_int t] reads untagged int pointed to by [t] and returns the corresponding tagged
    int. This should only be used to read a value written by [store_untagged_int].
    Otherwise, if the value has most significant bit set, it will be lost by tagging. To
    avoid it, use [load_unboxed_nativeint] and check before converting to int (should not
    allocate). The native C stub is the same for both. *)
external load_untagged_int
  :  t
  -> (int[@untagged])
  = "caml_ext_pointer_load_untagged_int" "caml_ext_pointer_load_unboxed_nativeint"
[@@noalloc]

(** [store_int t d] untags [d] and stores the result to the memory pointed to by [t]. *)
external store_untagged_int
  :  t
  -> (int[@untagged])
  -> unit
  = "caml_ext_pointer_store_untagged_int" "caml_ext_pointer_store_unboxed_nativeint"
[@@noalloc]

(** [load_unboxed_nativeint t] reads unboxed nativeint pointed to by [t] and returns the
    corresponding (boxed) nativeint allocated on the OCaml heap. *)
external load_unboxed_nativeint
  :  t
  -> (nativeint[@unboxed])
  = "caml_ext_pointer_load_unboxed_nativeint_bytecode"
    "caml_ext_pointer_load_unboxed_nativeint"
[@@noalloc]

(** [store_unboxed_nativeint t d] stores the unboxed nativeint to the memory pointed to by
    [t]. *)
external store_unboxed_nativeint
  :  t
  -> (nativeint[@unboxed])
  -> unit
  = "caml_ext_pointer_store_unboxed_nativeint_bytecode"
    "caml_ext_pointer_store_unboxed_nativeint"
[@@noalloc]

(** [load_unboxed_int64 t] reads unboxed int64 pointed to by [t] and returns the
    corresponding (boxed) int64 allocated on the OCaml heap. *)
external load_unboxed_int64
  :  t
  -> (int64[@unboxed])
  = "caml_ext_pointer_load_unboxed_int64_bytecode" "caml_ext_pointer_load_unboxed_int64"
[@@noalloc]

(** [store_unboxed_int64 t d] stores the unboxed int64 to the memory pointed to by [t]. *)
external store_unboxed_int64
  :  t
  -> (int64[@unboxed])
  -> unit
  = "caml_ext_pointer_store_unboxed_int64_bytecode" "caml_ext_pointer_store_unboxed_int64"
[@@noalloc]

(** [load_unboxed_int32 t] reads unboxed int32 pointed to by [t] and returns the
    corresponding (boxed) int32 allocated on the OCaml heap. *)
external load_unboxed_int32
  :  t
  -> (int32[@unboxed])
  = "caml_ext_pointer_load_unboxed_int32_bytecode" "caml_ext_pointer_load_unboxed_int32"
[@@noalloc]

(** [store_unboxed_int32 t d] stores the unboxed int32 to the memory pointed to by [t]. *)
external store_unboxed_int32
  :  t
  -> (int32[@unboxed])
  -> unit
  = "caml_ext_pointer_store_unboxed_int32_bytecode" "caml_ext_pointer_store_unboxed_int32"
[@@noalloc]

(** For float operations, the pointer must be aligned at least to the native integer
    machine width (meaning on 32-bit platforms, a 32-bit-aligned pointer is acceptable
    even though the width of the float is 64 bits). *)

(** [load_unboxed_float t] reads the unboxed float pointed to by [t]. (If the result is
    not directly passed to another operation expecting an unboxed float, then it will be
    boxed.) *)
external load_unboxed_float
  :  t
  -> (float[@unboxed])
  = "caml_ext_pointer_load_unboxed_float_bytecode" "caml_ext_pointer_load_unboxed_float"
[@@noalloc]

(** [store_unboxed_float t d] stores the unboxed float to the memory pointed to by [t]. *)
external store_unboxed_float
  :  t
  -> (float[@unboxed])
  -> unit
  = "caml_ext_pointer_store_unboxed_float_bytecode" "caml_ext_pointer_store_unboxed_float"
[@@noalloc]

(** Intrinsics for unboxed types of arguments or results. *)
module Unboxed = struct
  external load_unboxed_nativeint
    :  t
    -> (nativeint#[@unboxed])
    = "caml_ext_pointer_load_unboxed_nativeint_bytecode"
      "caml_ext_pointer_load_unboxed_nativeint"
  [@@noalloc]

  external store_unboxed_nativeint
    :  t
    -> (nativeint#[@unboxed])
    -> unit
    = "caml_ext_pointer_store_unboxed_nativeint_bytecode"
      "caml_ext_pointer_store_unboxed_nativeint"
  [@@noalloc]

  external load_unboxed_int64
    :  t
    -> (int64#[@unboxed])
    = "caml_ext_pointer_load_unboxed_int64_bytecode" "caml_ext_pointer_load_unboxed_int64"
  [@@noalloc]

  external store_unboxed_int64
    :  t
    -> (int64#[@unboxed])
    -> unit
    = "caml_ext_pointer_store_unboxed_int64_bytecode"
      "caml_ext_pointer_store_unboxed_int64"
  [@@noalloc]

  external load_unboxed_int32
    :  t
    -> (int32#[@unboxed])
    = "caml_ext_pointer_load_unboxed_int32_bytecode" "caml_ext_pointer_load_unboxed_int32"
  [@@noalloc]

  external store_unboxed_int32
    :  t
    -> (int32#[@unboxed])
    -> unit
    = "caml_ext_pointer_store_unboxed_int32_bytecode"
      "caml_ext_pointer_store_unboxed_int32"
  [@@noalloc]

  external load_unboxed_float
    :  t
    -> (float#[@unboxed])
    = "caml_ext_pointer_load_unboxed_float_bytecode" "caml_ext_pointer_load_unboxed_float"
  [@@noalloc]

  external store_unboxed_float
    :  t
    -> (float#[@unboxed])
    -> unit
    = "caml_ext_pointer_store_unboxed_float_bytecode"
      "caml_ext_pointer_store_unboxed_float"
  [@@noalloc]
end
