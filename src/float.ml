external round
  :  (int[@untagged])
  -> (float[@unboxed])
  -> (float[@unboxed])
  @@ portable
  = "caml_sse41_float64_round_bytecode" "caml_sse41_float64_round"
[@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]

module Rounding_mode = struct
  (* These also imply _MM_FROUND_NO_EXC *)
  let nearest = 0x8
  let neg_inf = 0x9
  let pos_inf = 0xA
  let zero = 0xB
  let current = 0xC
end

let[@inline always] round_half_to_even x = round Rounding_mode.current x
let[@inline always] round_down x = round Rounding_mode.neg_inf x
let[@inline always] round_up x = round Rounding_mode.pos_inf x
let[@inline always] round_towards_zero x = round Rounding_mode.zero x
let[@inline always] round_nearest x = round Rounding_mode.nearest x

include Ocaml_intrinsics_kernel.Float

module Unboxed = struct
  include Unboxed

  external round
    :  (int[@untagged])
    -> (float#[@unboxed])
    -> (float#[@unboxed])
    @@ portable
    = "caml_sse41_float64_round_bytecode" "caml_sse41_float64_round"
  [@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]

  let[@inline always] round_half_to_even x = round Rounding_mode.current x
  let[@inline always] round_down x = round Rounding_mode.neg_inf x
  let[@inline always] round_up x = round Rounding_mode.pos_inf x
  let[@inline always] round_towards_zero x = round Rounding_mode.zero x
  let[@inline always] round_nearest x = round Rounding_mode.nearest x
end
