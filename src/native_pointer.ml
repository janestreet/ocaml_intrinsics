type t = private nativeint

external ext_pointer_as_native_pointer
  :  int
  -> (t[@unboxed])
  = "caml_ext_pointer_as_native_pointer_bytecode" "caml_ext_pointer_as_native_pointer"
  [@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]

external unsafe_of_value
  :  'a
  -> (t[@unboxed])
  = "caml_native_pointer_of_value_bytecode" "caml_native_pointer_of_value"
  [@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]

external unsafe_to_value
  :  (t[@unboxed])
  -> 'a
  = "caml_native_pointer_to_value_bytecode" "caml_native_pointer_to_value"
  [@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]

external load_untagged_int
  :  (t[@unboxed])
  -> (int[@untagged])
  = "caml_native_pointer_load_untagged_int_bytecode"
    "caml_native_pointer_load_unboxed_nativeint"
  [@@noalloc] [@@builtin] [@@no_effects]

external store_untagged_int
  :  (t[@unboxed])
  -> (int[@untagged])
  -> unit
  = "caml_native_pointer_store_untagged_int_bytecode"
    "caml_native_pointer_store_unboxed_nativeint"
  [@@noalloc] [@@builtin] [@@no_coeffects]

external load_unboxed_nativeint
  :  t
  -> nativeint
  = "caml_native_pointer_load_unboxed_nativeint_bytecode"
    "caml_native_pointer_load_unboxed_nativeint"
  [@@unboxed] [@@noalloc] [@@builtin] [@@no_effects]

external store_unboxed_nativeint
  :  (t[@unboxed])
  -> (nativeint[@unboxed])
  -> unit
  = "caml_native_pointer_store_unboxed_nativeint_bytecode"
    "caml_native_pointer_store_unboxed_nativeint"
  [@@noalloc] [@@builtin] [@@no_coeffects]

external load_unboxed_int64
  :  t
  -> int64
  = "caml_native_pointer_load_unboxed_int64_bytecode"
    "caml_native_pointer_load_unboxed_int64"
  [@@unboxed] [@@noalloc] [@@builtin] [@@no_effects]

external store_unboxed_int64
  :  (t[@unboxed])
  -> (int64[@unboxed])
  -> unit
  = "caml_native_pointer_store_unboxed_int64_bytecode"
    "caml_native_pointer_store_unboxed_int64"
  [@@noalloc] [@@builtin] [@@no_coeffects]

external load_unboxed_int32
  :  t
  -> int32
  = "caml_native_pointer_load_unboxed_int32_bytecode"
    "caml_native_pointer_load_unboxed_int32"
  [@@unboxed] [@@noalloc] [@@builtin] [@@no_effects]

external store_unboxed_int32
  :  (t[@unboxed])
  -> (int32[@unboxed])
  -> unit
  = "caml_native_pointer_store_unboxed_int32_bytecode"
    "caml_native_pointer_store_unboxed_int32"
  [@@noalloc] [@@builtin] [@@no_coeffects]

external load_unboxed_float
  :  t
  -> float
  = "caml_native_pointer_load_unboxed_float_bytecode"
    "caml_native_pointer_load_unboxed_float"
  [@@unboxed] [@@noalloc] [@@builtin] [@@no_effects]

external store_unboxed_float
  :  (t[@unboxed])
  -> (float[@unboxed])
  -> unit
  = "caml_native_pointer_store_unboxed_float_bytecode"
    "caml_native_pointer_store_unboxed_float"
  [@@noalloc] [@@builtin] [@@no_coeffects]

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

module Immediate (V : sig
  type t [@@immediate64]
end) : Immediate_intf with module V = V = struct
  module V = V

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

module Int = Immediate (Stdlib.Int)
module Bool = Immediate (Stdlib.Bool)

module Expert = struct
  external of_nativeint : nativeint -> t = "%identity"
  external to_nativeint : t -> nativeint = "%identity"
end

open Expert
module NI = Stdlib.Nativeint

let advance (t : t) ~(bytes : nativeint) : t =
  of_nativeint (NI.add (to_nativeint t) bytes)
;;

let difference_in_bytes (start : t) (stop : t) : nativeint =
  NI.sub (to_nativeint stop) (to_nativeint start)
;;

let ( < ) (l : t) (r : t) = NI.compare (to_nativeint l) (to_nativeint r) < 0
let ( > ) (l : t) (r : t) = NI.compare (to_nativeint l) (to_nativeint r) > 0
let ( <= ) (l : t) (r : t) = l < r || l = r
let ( >= ) (l : t) (r : t) = l > r || l = r
let ( = ) (l : t) (r : t) = NI.equal (to_nativeint l) (to_nativeint r)
let ( <> ) (l : t) (r : t) = not (l = r)
