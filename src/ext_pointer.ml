type t = private int

external create : int -> t @@ portable = "%identity"

let offset_by_2n_bytes (t : t) n =
  (* Because the least significant bit of the pointer is implicitly zero, adding to the
     represented integer value adds twice as much to the represented pointer value.
     Argument is doubled to ensure that the resulting address is 2-byte aligned. Checking
     that the argument is even would result in worse code generation with a conditional
     branch. *)
  create ((t :> int) + n)
;;

module type Immediate = sig
  type imm : immediate

  val unsafe_load_immediate : t -> imm
  val store_immediate : t -> imm -> unit
end

module Immediate (V : sig
    type t : immediate
  end) =
struct
  external unsafe_load_immediate
    :  t
    -> V.t
    @@ portable
    = "caml_ext_pointer_load_immediate"
  [@@noalloc] [@@no_effects]

  external store_immediate
    :  t
    -> V.t
    -> unit
    @@ portable
    = "caml_ext_pointer_store_immediate"
  [@@noalloc] [@@no_coeffects]
end

module Int = Immediate (Stdlib.Int)
module Bool = Immediate (Stdlib.Bool)

external load_untagged_int
  :  t
  -> (int[@untagged])
  @@ portable
  = "caml_ext_pointer_load_untagged_int" "caml_ext_pointer_load_unboxed_nativeint"
[@@noalloc] [@@no_effects]

external store_untagged_int
  :  t
  -> (int[@untagged])
  -> unit
  @@ portable
  = "caml_ext_pointer_store_untagged_int" "caml_ext_pointer_store_unboxed_nativeint"
[@@noalloc] [@@no_coeffects]

external load_unboxed_nativeint
  :  t
  -> (nativeint[@unboxed])
  @@ portable
  = "caml_ext_pointer_load_unboxed_nativeint_bytecode"
    "caml_ext_pointer_load_unboxed_nativeint"
[@@noalloc] [@@no_effects]

external store_unboxed_nativeint
  :  t
  -> (nativeint[@unboxed])
  -> unit
  @@ portable
  = "caml_ext_pointer_store_unboxed_nativeint_bytecode"
    "caml_ext_pointer_store_unboxed_nativeint"
[@@noalloc] [@@no_coeffects]

external load_unboxed_int64
  :  t
  -> (int64[@unboxed])
  @@ portable
  = "caml_ext_pointer_load_unboxed_int64_bytecode" "caml_ext_pointer_load_unboxed_int64"
[@@noalloc] [@@no_effects]

external store_unboxed_int64
  :  t
  -> (int64[@unboxed])
  -> unit
  @@ portable
  = "caml_ext_pointer_store_unboxed_int64_bytecode" "caml_ext_pointer_store_unboxed_int64"
[@@noalloc] [@@no_coeffects]

external load_unboxed_int32
  :  t
  -> (int32[@unboxed])
  @@ portable
  = "caml_ext_pointer_load_unboxed_int32_bytecode" "caml_ext_pointer_load_unboxed_int32"
[@@noalloc] [@@no_effects]

external store_unboxed_int32
  :  t
  -> (int32[@unboxed])
  -> unit
  @@ portable
  = "caml_ext_pointer_store_unboxed_int32_bytecode" "caml_ext_pointer_store_unboxed_int32"
[@@noalloc] [@@no_coeffects]

external load_unboxed_float
  :  t
  -> (float[@unboxed])
  @@ portable
  = "caml_ext_pointer_load_unboxed_float_bytecode" "caml_ext_pointer_load_unboxed_float"
[@@noalloc] [@@no_effects]

external store_unboxed_float
  :  t
  -> (float[@unboxed])
  -> unit
  @@ portable
  = "caml_ext_pointer_store_unboxed_float_bytecode" "caml_ext_pointer_store_unboxed_float"
[@@noalloc] [@@no_coeffects]

module Unboxed = struct
  external load_unboxed_nativeint
    :  t
    -> (nativeint#[@unboxed])
    @@ portable
    = "caml_ext_pointer_load_unboxed_nativeint_bytecode"
      "caml_ext_pointer_load_unboxed_nativeint"
  [@@noalloc] [@@no_effects]

  external store_unboxed_nativeint
    :  t
    -> (nativeint#[@unboxed])
    -> unit
    @@ portable
    = "caml_ext_pointer_store_unboxed_nativeint_bytecode"
      "caml_ext_pointer_store_unboxed_nativeint"
  [@@noalloc] [@@no_coeffects]

  external load_unboxed_int64
    :  t
    -> (int64#[@unboxed])
    @@ portable
    = "caml_ext_pointer_load_unboxed_int64_bytecode" "caml_ext_pointer_load_unboxed_int64"
  [@@noalloc] [@@no_effects]

  external store_unboxed_int64
    :  t
    -> (int64#[@unboxed])
    -> unit
    @@ portable
    = "caml_ext_pointer_store_unboxed_int64_bytecode"
      "caml_ext_pointer_store_unboxed_int64"
  [@@noalloc] [@@no_coeffects]

  external load_unboxed_int32
    :  t
    -> (int32#[@unboxed])
    @@ portable
    = "caml_ext_pointer_load_unboxed_int32_bytecode" "caml_ext_pointer_load_unboxed_int32"
  [@@noalloc] [@@no_effects]

  external store_unboxed_int32
    :  t
    -> (int32#[@unboxed])
    -> unit
    @@ portable
    = "caml_ext_pointer_store_unboxed_int32_bytecode"
      "caml_ext_pointer_store_unboxed_int32"
  [@@noalloc] [@@no_coeffects]

  external load_unboxed_float
    :  t
    -> (float#[@unboxed])
    @@ portable
    = "caml_ext_pointer_load_unboxed_float_bytecode" "caml_ext_pointer_load_unboxed_float"
  [@@noalloc] [@@no_effects]

  external store_unboxed_float
    :  t
    -> (float#[@unboxed])
    -> unit
    @@ portable
    = "caml_ext_pointer_store_unboxed_float_bytecode"
      "caml_ext_pointer_store_unboxed_float"
  [@@noalloc] [@@no_coeffects]
end
