include Ocaml_intrinsics_kernel.Float

let round_half_to_even = round_current
let iround_half_to_even = iround_current

module Unboxed = struct
  include Unboxed

  let round_half_to_even = round_current
  let iround_half_to_even = iround_current
end
