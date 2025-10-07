include Ocaml_intrinsics_kernel_test.Import

module type String_any = sig
  type t : any

  val to_string : t -> string
end

module type V = sig
  type t : any

  include String_any with type t := t

  module Hex_unsigned : sig
    val to_string : t -> string
  end
end

let%template[@kind k = (bits64, bits32, word)] test_op
  (type t : k)
  ~op
  ~op_name
  ~of_t
  (module V : V with type t = t)
  t
  =
  let x = of_t t in
  print_endline [%string "%{op_name} %{x#V.Hex_unsigned} = %{op x#V}"]
;;
