module Expert = Atomic_expert

module Native_pointer = struct
  let fetch_and_add = Expert.native_pointer_fetch_and_add_int
  let fetch_and_sub = Expert.native_pointer_fetch_and_sub_int
  let fetch_and_add_int64 = Expert.native_pointer_fetch_and_add_int64
  let fetch_and_sub_int64 = Expert.native_pointer_fetch_and_sub_int64
  let fetch_and_add_int32 = Expert.native_pointer_fetch_and_add_int32
  let fetch_and_sub_int32 = Expert.native_pointer_fetch_and_sub_int32
  let fetch_and_add_nativeint = Expert.native_pointer_fetch_and_add_nativeint
  let fetch_and_sub_nativeint = Expert.native_pointer_fetch_and_sub_nativeint
  let compare_and_swap = Expert.native_pointer_compare_and_swap_int
  let compare_and_swap_int64 = Expert.native_pointer_compare_and_swap_int64
  let compare_and_swap_int32 = Expert.native_pointer_compare_and_swap_int32
  let compare_and_swap_nativeint = Expert.native_pointer_compare_and_swap_nativeint
end

module Ext_pointer = struct
  let fetch_and_add = Expert.ext_pointer_fetch_and_add_int
  let fetch_and_sub = Expert.ext_pointer_fetch_and_sub_int
  let fetch_and_add_int64 = Expert.ext_pointer_fetch_and_add_int64
  let fetch_and_sub_int64 = Expert.ext_pointer_fetch_and_sub_int64
  let fetch_and_add_int32 = Expert.ext_pointer_fetch_and_add_int32
  let fetch_and_sub_int32 = Expert.ext_pointer_fetch_and_sub_int32
  let fetch_and_add_nativeint = Expert.ext_pointer_fetch_and_add_nativeint
  let fetch_and_sub_nativeint = Expert.ext_pointer_fetch_and_sub_nativeint
  let compare_and_swap = Expert.ext_pointer_compare_and_swap_int
  let compare_and_swap_int64 = Expert.ext_pointer_compare_and_swap_int64
  let compare_and_swap_int32 = Expert.ext_pointer_compare_and_swap_int32
  let compare_and_swap_nativeint = Expert.ext_pointer_compare_and_swap_nativeint
end

module Bigstring = struct
  let fetch_and_add = Expert.bigstring_fetch_and_add_int
  let fetch_and_sub = Expert.bigstring_fetch_and_sub_int
  let fetch_and_add_int64 = Expert.bigstring_fetch_and_add_int64
  let fetch_and_sub_int64 = Expert.bigstring_fetch_and_sub_int64
  let fetch_and_add_int32 = Expert.bigstring_fetch_and_add_int32
  let fetch_and_sub_int32 = Expert.bigstring_fetch_and_sub_int32
  let fetch_and_add_nativeint = Expert.bigstring_fetch_and_add_nativeint
  let fetch_and_sub_nativeint = Expert.bigstring_fetch_and_sub_nativeint
  let compare_and_swap = Expert.bigstring_compare_and_swap_int
  let compare_and_swap_int64 = Expert.bigstring_compare_and_swap_int64
  let compare_and_swap_int32 = Expert.bigstring_compare_and_swap_int32
  let compare_and_swap_nativeint = Expert.bigstring_compare_and_swap_nativeint
end
