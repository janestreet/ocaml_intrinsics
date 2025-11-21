type t = nativeint#

let arch_big_endian = Stdlib.Sys.big_endian

external int64_to_int : local_ int64 -> int = "%int64_to_int"
external int64_of_int : int -> local_ int64 = "%int64_of_int"
external swap64 : local_ int64 -> local_ int64 = "%bswap_int64"

external ext_pointer_as_native_pointer
  :  int
  -> (t[@unboxed])
  = "caml_ext_pointer_as_native_pointer_bytecode" "caml_ext_pointer_as_native_pointer"
[@@noalloc]

external unsafe_of_value
  : ('a : value_or_null).
  'a @ local -> (t[@unboxed])
  @@ portable
  = "caml_native_pointer_of_value_bytecode" "caml_native_pointer_of_value"
[@@noalloc]

external unsafe_to_value
  : ('a : value_or_null).
  (t[@unboxed]) -> 'a
  @@ portable
  = "caml_native_pointer_to_value_bytecode" "caml_native_pointer_to_value"
[@@noalloc]

external unsafe_of_bigstring
  :  Bigstring_intf.t
  -> pos:(int[@untagged])
  -> (t[@unboxed])
  = "caml_native_pointer_of_bigstring_bytecode" "caml_native_pointer_of_bigstring"
[@@noalloc]

external load_untagged_char
  :  (t[@unboxed])
  -> (char[@untagged])
  = "caml_native_pointer_load_untagged_char_bytecode"
    "caml_native_pointer_load_untagged_char"
[@@noalloc]

external store_untagged_char
  :  (t[@unboxed])
  -> (char[@untagged])
  -> unit
  = "caml_native_pointer_store_untagged_char_bytecode"
    "caml_native_pointer_store_untagged_char"
[@@noalloc]

external load_untagged_int
  :  (t[@unboxed])
  -> (int[@untagged])
  = "caml_native_pointer_load_untagged_int_bytecode"
    "caml_native_pointer_load_unboxed_nativeint"
[@@noalloc]

external store_untagged_int
  :  (t[@unboxed])
  -> (int[@untagged])
  -> unit
  = "caml_native_pointer_store_untagged_int_bytecode"
    "caml_native_pointer_store_unboxed_nativeint"
[@@noalloc]

external load_unboxed_nativeint
  :  t
  -> nativeint
  = "caml_native_pointer_load_unboxed_nativeint_bytecode"
    "caml_native_pointer_load_unboxed_nativeint"
[@@unboxed] [@@noalloc]

external store_unboxed_nativeint
  :  (t[@unboxed])
  -> local_ (nativeint[@unboxed])
  -> unit
  = "caml_native_pointer_store_unboxed_nativeint_bytecode"
    "caml_native_pointer_store_unboxed_nativeint"
[@@noalloc]

external load_unboxed_int64
  :  t
  -> (int64[@local_opt])
  = "caml_native_pointer_load_unboxed_int64_bytecode"
    "caml_native_pointer_load_unboxed_int64"
[@@unboxed] [@@noalloc]

external store_unboxed_int64
  :  (t[@unboxed])
  -> local_ (int64[@unboxed])
  -> unit
  = "caml_native_pointer_store_unboxed_int64_bytecode"
    "caml_native_pointer_store_unboxed_int64"
[@@noalloc]

external load_unboxed_int32
  :  t
  -> int32
  = "caml_native_pointer_load_unboxed_int32_bytecode"
    "caml_native_pointer_load_unboxed_int32"
[@@unboxed] [@@noalloc]

external store_unboxed_int32
  :  (t[@unboxed])
  -> local_ (int32[@unboxed])
  -> unit
  = "caml_native_pointer_store_unboxed_int32_bytecode"
    "caml_native_pointer_store_unboxed_int32"
[@@noalloc]

external load_unboxed_float
  :  t
  -> float
  = "caml_native_pointer_load_unboxed_float_bytecode"
    "caml_native_pointer_load_unboxed_float"
[@@unboxed] [@@noalloc]

external store_unboxed_float
  :  (t[@unboxed])
  -> local_ (float[@unboxed])
  -> unit
  = "caml_native_pointer_store_unboxed_float_bytecode"
    "caml_native_pointer_store_unboxed_float"
[@@noalloc]

let[@inline always] [@zero_alloc] unsafe_load_int64_int t =
  int64_to_int (load_unboxed_int64 t)
;;

let[@inline always] [@zero_alloc] unsafe_load_int64_int_swap t =
  int64_to_int (swap64 (load_unboxed_int64 t))
;;

let[@inline always] [@zero_alloc] unsafe_write_int64_int_swap t x =
  store_unboxed_int64 t (swap64 (int64_of_int x))
;;

let[@inline always] [@zero_alloc] unsafe_write_int64_int t x =
  store_unboxed_int64 t (int64_of_int x)
;;

let[@inline always] [@zero_alloc] unsafe_load_int64_le_trunc t =
  if arch_big_endian then unsafe_load_int64_int_swap t else unsafe_load_int64_int t
;;

let[@inline always] [@zero_alloc] unsafe_store_int64_le t x =
  if arch_big_endian then unsafe_write_int64_int_swap t x else unsafe_write_int64_int t x
;;

external unsafe_blit_to_bigstring
  :  src:(t[@unboxed])
  -> src_pos:(int[@untagged])
  -> dst:Bigstring_intf.t
  -> dst_pos:(int[@untagged])
  -> len:(int[@untagged])
  -> unit
  = "caml_native_pointer_unsafe_blit_to_bigstring_bytecode"
    "caml_native_pointer_unsafe_blit_to_bigstring"
[@@noalloc]

external unsafe_blit
  :  src:(t[@unboxed])
  -> src_pos:(int[@untagged])
  -> dst:(t[@unboxed])
  -> dst_pos:(int[@untagged])
  -> len:(int[@untagged])
  -> unit
  = "caml_native_pointer_unsafe_blit_bytecode" "caml_native_pointer_unsafe_blit"
[@@noalloc]

external unsafe_memset
  :  (t[@unboxed])
  -> (char[@untagged])
  -> pos:(int[@untagged])
  -> len:(int[@untagged])
  -> unit
  = "caml_native_pointer_unsafe_memset_bytecode" "caml_native_pointer_unsafe_memset"
[@@noalloc]

[@@@warning "-incompatible-with-upstream"]

module Unboxed = struct
  external load_unboxed_nativeint
    :  t
    -> nativeint#
    = "caml_native_pointer_load_unboxed_nativeint_bytecode"
      "caml_native_pointer_load_unboxed_nativeint"
  [@@unboxed] [@@noalloc] [@@builtin] [@@no_effects]

  external store_unboxed_nativeint
    : ('a : word).
    (t[@unboxed]) -> local_ ('a[@unboxed]) -> unit
    = "caml_native_pointer_store_unboxed_nativeint_bytecode"
      "caml_native_pointer_store_unboxed_nativeint"
  [@@noalloc] [@@builtin] [@@no_coeffects]

  external load_unboxed_int64
    :  t
    -> int64#
    = "caml_native_pointer_load_unboxed_int64_bytecode"
      "caml_native_pointer_load_unboxed_int64"
  [@@unboxed] [@@noalloc] [@@builtin] [@@no_effects]

  external store_unboxed_int64
    : ('a : bits64).
    (t[@unboxed]) -> local_ ('a[@unboxed]) -> unit
    = "caml_native_pointer_store_unboxed_int64_bytecode"
      "caml_native_pointer_store_unboxed_int64"
  [@@noalloc] [@@builtin] [@@no_coeffects]

  external load_unboxed_int32
    :  t
    -> int32#
    = "caml_native_pointer_load_unboxed_int32_bytecode"
      "caml_native_pointer_load_unboxed_int32"
  [@@unboxed] [@@noalloc] [@@builtin] [@@no_effects]

  external store_unboxed_int32
    : ('a : bits32).
    (t[@unboxed]) -> local_ ('a[@unboxed]) -> unit
    = "caml_native_pointer_store_unboxed_int32_bytecode"
      "caml_native_pointer_store_unboxed_int32"
  [@@noalloc] [@@builtin] [@@no_coeffects]

  external load_unboxed_float
    :  t
    -> float#
    = "caml_native_pointer_load_unboxed_float_bytecode"
      "caml_native_pointer_load_unboxed_float"
  [@@unboxed] [@@noalloc] [@@builtin] [@@no_effects]

  external store_unboxed_float
    : ('a : float64).
    (t[@unboxed]) -> local_ ('a[@unboxed]) -> unit
    = "caml_native_pointer_store_unboxed_float_bytecode"
      "caml_native_pointer_store_unboxed_float"
  [@@noalloc] [@@builtin] [@@no_coeffects]

  module Unchecked = struct
    external load_unboxed_nativeint
      : ('a : word).
      t -> 'a
      = "caml_native_pointer_load_unboxed_nativeint_bytecode"
        "caml_native_pointer_load_unboxed_nativeint"
    [@@unboxed] [@@noalloc] [@@builtin] [@@no_effects]

    external load_unboxed_int64
      : ('a : bits64).
      t -> 'a
      = "caml_native_pointer_load_unboxed_int64_bytecode"
        "caml_native_pointer_load_unboxed_int64"
    [@@unboxed] [@@noalloc] [@@builtin] [@@no_effects]

    external load_unboxed_int32
      : ('a : bits32).
      t -> 'a
      = "caml_native_pointer_load_unboxed_int32_bytecode"
        "caml_native_pointer_load_unboxed_int32"
    [@@unboxed] [@@noalloc] [@@builtin] [@@no_effects]

    external load_unboxed_float
      : ('a : float64).
      t -> 'a
      = "caml_native_pointer_load_unboxed_float_bytecode"
        "caml_native_pointer_load_unboxed_float"
    [@@unboxed] [@@noalloc] [@@builtin] [@@no_effects]
  end
end
[@@ocaml.doc {| Intrinsics for unboxed types. |}]

module Unchecked = struct
  external load_immediate
    : ('a : immediate64).
    (t[@unboxed]) -> 'a
    = "caml_native_pointer_load_immediate_bytecode" "caml_native_pointer_load_immediate"
  [@@noalloc] [@@builtin] [@@no_effects]
end

external store_immediate
  : ('a : immediate64).
  (t[@unboxed]) -> 'a -> unit
  = "caml_native_pointer_store_immediate_bytecode" "caml_native_pointer_store_immediate"
[@@noalloc] [@@builtin] [@@no_coeffects]

module type Immediate_intf = sig
  module V : sig
    type t : immediate64
  end

  external unsafe_load_immediate
    :  (t[@unboxed])
    -> V.t
    = "caml_native_pointer_load_immediate_bytecode" "caml_native_pointer_load_immediate"
  [@@noalloc]

  external store_immediate
    :  (t[@unboxed])
    -> V.t
    -> unit
    = "caml_native_pointer_store_immediate_bytecode" "caml_native_pointer_store_immediate"
  [@@noalloc]
end

module Immediate (V : sig
    type t : immediate64
  end) : Immediate_intf with module V = V = struct
  module V = V

  external unsafe_load_immediate
    :  (t[@unboxed])
    -> V.t
    = "caml_native_pointer_load_immediate_bytecode" "caml_native_pointer_load_immediate"
  [@@noalloc]

  external store_immediate
    :  (t[@unboxed])
    -> V.t
    -> unit
    = "caml_native_pointer_store_immediate_bytecode" "caml_native_pointer_store_immediate"
  [@@noalloc]
end

module Int = Immediate (Stdlib.Int)
module Bool = Immediate (Stdlib.Bool)

module Expert = struct
  external of_nativeint
    :  (nativeint[@local_opt])
    -> (t[@unboxed])
    @@ portable
    = "%unbox_nativeint"

  external to_nativeint : (t[@unboxed]) -> (nativeint[@local_opt]) = "%box_nativeint"
end

open Expert
module NI = Stdlib.Nativeint

let[@inline] [@zero_alloc] advance (t : t) ~(bytes : nativeint#) : t =
  of_nativeint (NI.add (to_nativeint t) (to_nativeint bytes))
;;

let difference_in_bytes (start : t) (stop : t) : nativeint =
  NI.sub (to_nativeint stop) (to_nativeint start)
;;

let equal (l : t) (r : t) = NI.equal (to_nativeint l) (to_nativeint r)
let compare (l : t) (r : t) = NI.compare (to_nativeint l) (to_nativeint r)
let ( < ) (l : t) (r : t) = (compare [@inlined]) l r < 0
let ( > ) (l : t) (r : t) = (compare [@inlined]) l r > 0
let ( = ) = equal
let ( <= ) (l : t) (r : t) = l < r || l = r
let ( >= ) (l : t) (r : t) = l > r || l = r
let ( <> ) (l : t) (r : t) = not (l = r)
