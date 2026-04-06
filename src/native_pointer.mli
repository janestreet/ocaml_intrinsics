@@ portable

(** [Native_pointer] uses Nativeint to hold a pointer to a memory block allocated outside
    the OCaml heap. The pointer is not required to be aligned. *)
type t = private nativeint#

(** [ext_pointer_as_native_pointer p] takes an int [p] that encodes a pointer to a memory
    block outside of the OCaml heap, decodes [p] by clearing the least significant bit of
    [p], and boxes the result as [nativeint]. Unlike untagging, decoding [p] does not
    shift the bits of [p]. *)
val ext_pointer_as_native_pointer : int -> t
[@@zero_alloc]

(** Reinterpret any 'a as a native pointer. The unboxed result will have the same bit
    representation as the input value. If 'a is a pointer to outside the OCaml heap, the
    result may be used with load or store operations. *)
val unsafe_of_value : ('a : value_or_null). 'a @ local -> t
[@@zero_alloc]

(** Reinterpret the unboxed contents of a native pointer as a value of any type. The
    result will have the same bit representation as the unboxed input pointer. The result
    must be a valid OCaml value: either an immediate or a pointer to an address with a
    valid OCaml header. *)
val unsafe_to_value : ('a : value_or_null). t -> 'a @@ portable
[@@zero_alloc]

(** Returns a [Native_pointer] to the underlying data of a [Bigstring]. *)
val unsafe_of_bigstring : Bigstring_intf.t -> pos:int -> t
[@@zero_alloc]

(** [load_untagged_char t] reads untagged char pointed to by [t] and returns the
    corresponding tagged char. *)
val load_untagged_char : t -> char
[@@zero_alloc]

(** [store_untagged_char t c] untags [c] and stores the result to the memory pointed to by
    [t]. *)
val store_untagged_char : t -> char -> unit
[@@zero_alloc]

(** [load_untagged_int t] reads untagged int pointed to by [t] and returns the
    corresponding tagged int. This should only be used to read a value written by
    [store_untagged_int]. Otherwise, if the value has most significant bit set, it will be
    lost by tagging. To avoid it, use [load_unboxed_nativeint] and check before converting
    to int (should not allocate). Their native code C stub is the same. *)
val load_untagged_int : t -> int
[@@zero_alloc]

(** [store_untagged_int t d] untags [d] and stores the result to the memory pointed to by
    [t]. *)
val store_untagged_int : t -> int -> unit
[@@zero_alloc]

(** [load_unboxed_nativeint t] reads unboxed nativeint pointed to by [t] and returns the
    corresponding (boxed) nativeint allocated on the OCaml heap. *)
val load_unboxed_nativeint : t -> nativeint

(** [store_unboxed_nativeint t d] stores the unboxed nativeint to the memory pointed to by
    [t]. *)
val store_unboxed_nativeint : t -> nativeint @ local -> unit

(** [load_unboxed_int64 t] reads unboxed int64 pointed to by [t] and returns the
    corresponding (boxed) int64 allocated on the OCaml heap. *)
val load_unboxed_int64 : t -> int64

(** [store_unboxed_int64 t d] stores the unboxed int64 to the memory pointed to by [t]. *)
val store_unboxed_int64 : t -> int64 @ local -> unit

(** [load_unboxed_int32 t] reads unboxed int32 pointed to by [t] and returns the
    corresponding (boxed) int32 allocated on the OCaml heap. *)
val load_unboxed_int32 : t -> int32

(** [store_unboxed_int32 t d] stores the unboxed int32 to the memory pointed to by [t]. *)
val store_unboxed_int32 : t -> int32 @ local -> unit

(** For float operations, the pointer must be aligned at least to the native integer
    machine width (meaning on 32-bit platforms, a 32-bit-aligned pointer is acceptable
    even though the width of the float is 64 bits). *)

(** [load_unboxed_float t] reads the unboxed float pointed to by [t]. (If the result is
    not directly passed to another operation expecting an unboxed float, then it will be
    boxed.) *)
val load_unboxed_float : t -> float

(** [store_unboxed_float t d] stores the unboxed float to the memory pointed to by [t]. *)
val store_unboxed_float : t -> float @ local -> unit

(** This function will read a 64 bit integer stored at [t], but will truncate the value
    since it returns an OCaml int to avoid allocation. *)
val unsafe_load_int64_le_trunc : t -> int
[@@zero_alloc]

(** This function assumes that the value being stored is an OCaml int, and thus takes up
    63 bits instead of 64. *)
val unsafe_store_int64_le : t -> int -> unit
[@@zero_alloc]

(** Blits data from a [Native_pointer] to a [Bigstring]. This is denoted as unsafe because
    unlike [Bigstring], no metadata is stored within a [Native_pointer], and so there are
    no bounds checking. *)
val unsafe_blit_to_bigstring
  :  src:t
  -> src_pos:int
  -> dst:Bigstring_intf.t
  -> dst_pos:int
  -> len:int
  -> unit
[@@zero_alloc]

(** Blits data from one [Native_pointer] to another. *)
val unsafe_blit : src:t -> src_pos:int -> dst:t -> dst_pos:int -> len:int -> unit
[@@zero_alloc]

val unsafe_memset : t -> char -> pos:int -> len:int -> unit [@@zero_alloc]

(** Pointer arithmetic and comparisons *)

(** Adds [n] bytes to a pointer, resulting in another pointer. *)
val advance : t -> bytes:nativeint# -> t
[@@zero_alloc]

(** [difference_in_bytes start end] subtracts start from end, assuming that both point to
    the same block of memory (such as an array). The result is the number of bytes, not a
    pointer. *)
val difference_in_bytes : t -> t -> nativeint

val equal : t -> t -> bool [@@zero_alloc]
val compare : t -> t -> int [@@zero_alloc]

(** Equal operator is defined for any two pointers. Inequality operators are only defined
    when both pointers point to the same block of memory (such as an array). *)
val ( = ) : t -> t -> bool
[@@zero_alloc]

val ( <> ) : t -> t -> bool [@@zero_alloc]
val ( < ) : t -> t -> bool [@@zero_alloc]
val ( > ) : t -> t -> bool [@@zero_alloc]
val ( <= ) : t -> t -> bool [@@zero_alloc]
val ( >= ) : t -> t -> bool [@@zero_alloc]

(** Load and store immediate values. Intended for use with [Int] and [Bool]. *)
module type Immediate_intf = sig @@ portable
  module V : sig
    type t : immediate64
  end

  val unsafe_load_immediate : t -> V.t [@@zero_alloc]
  val unsafe_store_immediate : t -> V.t -> unit [@@zero_alloc]
end

module Int : Immediate_intf with type V.t = Stdlib.Int.t
module Bool : Immediate_intf with type V.t = Stdlib.Bool.t

module Expert : sig
  external of_nativeint
    :  (nativeint[@local_opt])
    -> (t[@unboxed])
    @@ portable
    = "%unbox_nativeint"
  [@@ocaml.doc {| This converts between a native pointer and a boxed nativeint. |}]

  external to_nativeint : (t[@unboxed]) -> (nativeint[@local_opt]) = "%box_nativeint"
end
