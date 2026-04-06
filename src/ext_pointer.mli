@@ portable

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

val create : int -> t

(** [offset_by_2n_bytes t n] represents an external pointer to address [t + 2*n]. *)
val offset_by_2n_bytes : t -> int -> t

module type Immediate = sig
  type imm : immediate

  (** [load_immediate t] assumes without checking that the value pointed to by [t] is
      immediate. *)
  val unsafe_load_immediate : t -> imm

  (** [store_int t i] stores the immediate [i] to the memory address represented by [t]. *)
  val store_immediate : t -> imm -> unit
end

module Int : Immediate with type imm := int
module Bool : Immediate with type imm := bool

(** [load_int t] reads untagged int pointed to by [t] and returns the corresponding tagged
    int. This should only be used to read a value written by [store_untagged_int].
    Otherwise, if the value has most significant bit set, it will be lost by tagging. To
    avoid it, use [load_unboxed_nativeint] and check before converting to int (should not
    allocate). The native C stub is the same for both. *)
val load_untagged_int : t -> int

(** [store_int t d] untags [d] and stores the result to the memory pointed to by [t]. *)
val store_untagged_int : t -> int -> unit

(** [load_unboxed_nativeint t] reads unboxed nativeint pointed to by [t] and returns the
    corresponding (boxed) nativeint allocated on the OCaml heap. *)
val load_unboxed_nativeint : t -> nativeint

(** [store_unboxed_nativeint t d] stores the unboxed nativeint to the memory pointed to by
    [t]. *)
val store_unboxed_nativeint : t -> nativeint -> unit

(** [load_unboxed_int64 t] reads unboxed int64 pointed to by [t] and returns the
    corresponding (boxed) int64 allocated on the OCaml heap. *)
val load_unboxed_int64 : t -> int64

(** [store_unboxed_int64 t d] stores the unboxed int64 to the memory pointed to by [t]. *)
val store_unboxed_int64 : t -> int64 -> unit

(** [load_unboxed_int32 t] reads unboxed int32 pointed to by [t] and returns the
    corresponding (boxed) int32 allocated on the OCaml heap. *)
val load_unboxed_int32 : t -> int32

(** [store_unboxed_int32 t d] stores the unboxed int32 to the memory pointed to by [t]. *)
val store_unboxed_int32 : t -> int32 -> unit

(** For float operations, the pointer must be aligned at least to the native integer
    machine width (meaning on 32-bit platforms, a 32-bit-aligned pointer is acceptable
    even though the width of the float is 64 bits). *)

(** [load_unboxed_float t] reads the unboxed float pointed to by [t]. (If the result is
    not directly passed to another operation expecting an unboxed float, then it will be
    boxed.) *)
val load_unboxed_float : t -> float

(** [store_unboxed_float t d] stores the unboxed float to the memory pointed to by [t]. *)
val store_unboxed_float : t -> float -> unit

(** Intrinsics for unboxed types of arguments or results. *)
module Unboxed : sig
  val load_unboxed_nativeint : t -> nativeint#
  val store_unboxed_nativeint : t -> nativeint# -> unit
  val load_unboxed_int64 : t -> int64#
  val store_unboxed_int64 : t -> int64# -> unit
  val load_unboxed_int32 : t -> int32#
  val store_unboxed_int32 : t -> int32# -> unit
  val load_unboxed_float : t -> float#
  val store_unboxed_float : t -> float# -> unit
end
