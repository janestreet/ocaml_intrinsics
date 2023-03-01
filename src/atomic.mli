(** Atomic arithmetic operations *)

module Expert = Atomic_expert

module Native_pointer : sig
  (** [fetch_and_$op_$type ptr val] atomically runs [*( *$type )ptr $op val], stores the
      result at [ptr], and returns the original value at [ptr]. *)

  (** [compare_and_swap_$type ptr ~compare_with ~set_to] atomically checks if the value
      at [ptr] is equal to [compare_with]: if so, it stores [set_to] to [ptr] and returns
      true, otherwise returns false *)

  (** Pointer to untagged int *)

  val fetch_and_add : Native_pointer.t -> int -> int
  val fetch_and_sub : Native_pointer.t -> int -> int
  val compare_and_swap : Native_pointer.t -> compare_with:int -> set_to:int -> bool

  (** Pointer to unboxed int64 *)

  val fetch_and_add_int64 : Native_pointer.t -> int64 -> int64
  val fetch_and_sub_int64 : Native_pointer.t -> int64 -> int64

  val compare_and_swap_int64
    :  Native_pointer.t
    -> compare_with:int64
    -> set_to:int64
    -> bool

  (** Pointer to unboxed int32 *)

  val fetch_and_add_int32 : Native_pointer.t -> int32 -> int32
  val fetch_and_sub_int32 : Native_pointer.t -> int32 -> int32

  val compare_and_swap_int32
    :  Native_pointer.t
    -> compare_with:int32
    -> set_to:int32
    -> bool

  (** Pointer to unboxed nativeint *)

  val fetch_and_add_nativeint : Native_pointer.t -> nativeint -> nativeint
  val fetch_and_sub_nativeint : Native_pointer.t -> nativeint -> nativeint

  val compare_and_swap_nativeint
    :  Native_pointer.t
    -> compare_with:nativeint
    -> set_to:nativeint
    -> bool
end

module Ext_pointer : sig
  (** [fetch_and_$op_$type ptr val] atomically runs [*( *$type )ptr $op val], stores the
      result at [ptr], and returns the original value at [ptr]. *)

  (** Pointer to untagged int *)

  val fetch_and_add : Ext_pointer.t -> int -> int
  val fetch_and_sub : Ext_pointer.t -> int -> int
  val compare_and_swap : Ext_pointer.t -> compare_with:int -> set_to:int -> bool

  (** Pointer to unboxed int64 *)

  val fetch_and_add_int64 : Ext_pointer.t -> int64 -> int64
  val fetch_and_sub_int64 : Ext_pointer.t -> int64 -> int64
  val compare_and_swap_int64 : Ext_pointer.t -> compare_with:int64 -> set_to:int64 -> bool

  (** Pointer to unboxed int32 *)

  val fetch_and_add_int32 : Ext_pointer.t -> int32 -> int32
  val fetch_and_sub_int32 : Ext_pointer.t -> int32 -> int32
  val compare_and_swap_int32 : Ext_pointer.t -> compare_with:int32 -> set_to:int32 -> bool

  (** Pointer to unboxed nativeint *)

  val fetch_and_add_nativeint : Ext_pointer.t -> nativeint -> nativeint
  val fetch_and_sub_nativeint : Ext_pointer.t -> nativeint -> nativeint

  val compare_and_swap_nativeint
    :  Ext_pointer.t
    -> compare_with:nativeint
    -> set_to:nativeint
    -> bool
end

module Bigstring : sig
  (** [fetch_and_$op_$type ptr ~pos val] atomically runs [*( *$type )((ptr + pos) $op val],
      stores the result at [ptr + pos], and returns the original value at [ptr + pos].

      Note that [pos] is a byte offset into the bigstring. If the offset is not
      a multiple of the target int width in bytes, the operation will be unaligned
      and potentially *much* slower than the aligned case. *)

  (** Pointer to untagged int *)

  val fetch_and_add : Bigstring_intf.t -> pos:int -> int -> int
  val fetch_and_sub : Bigstring_intf.t -> pos:int -> int -> int

  val compare_and_swap
    :  Bigstring_intf.t
    -> pos:int
    -> compare_with:int
    -> set_to:int
    -> bool

  (** Pointer to unboxed int64 *)

  val fetch_and_add_int64 : Bigstring_intf.t -> pos:int -> int64 -> int64
  val fetch_and_sub_int64 : Bigstring_intf.t -> pos:int -> int64 -> int64

  val compare_and_swap_int64
    :  Bigstring_intf.t
    -> pos:int
    -> compare_with:int64
    -> set_to:int64
    -> bool

  (** Pointer to unboxed int32 *)

  val fetch_and_add_int32 : Bigstring_intf.t -> pos:int -> int32 -> int32
  val fetch_and_sub_int32 : Bigstring_intf.t -> pos:int -> int32 -> int32

  val compare_and_swap_int32
    :  Bigstring_intf.t
    -> pos:int
    -> compare_with:int32
    -> set_to:int32
    -> bool

  (** Pointer to unboxed nativeint *)

  val fetch_and_add_nativeint : Bigstring_intf.t -> pos:int -> nativeint -> nativeint
  val fetch_and_sub_nativeint : Bigstring_intf.t -> pos:int -> nativeint -> nativeint

  val compare_and_swap_nativeint
    :  Bigstring_intf.t
    -> pos:int
    -> compare_with:nativeint
    -> set_to:nativeint
    -> bool
end
