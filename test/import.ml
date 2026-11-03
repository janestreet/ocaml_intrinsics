module type String_any = sig
  type t

  val to_string : t -> string
end

module type V = sig
  type t

  include String_any with type t := t

  module Hex_unsigned : sig
    val to_string : t -> string
  end
end
