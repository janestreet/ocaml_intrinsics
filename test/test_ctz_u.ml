[%%import "config.h"]

open Base
module I = Ocaml_intrinsics
open Import

let numbers = [ 0 (* Int.num_bits *); 1; 7; 2; 4; 12; 18; -1 ]

let%expect_test "ctz int64" =
  let open Int64 in
  let numbers = List.map numbers ~f:of_int in
  let f =
    [%template test_op [@kind bits64]]
      (module Int64_u)
      ~op:I.Int64.Unboxed.count_trailing_zeros
      ~op_name:"ctz"
      ~of_t:Int64_u.of_int64
  in
  List.iter ~f (max_value :: min_value :: numbers);
  [%expect
    {|
    ctz 0x7fffffffffffffff = 0
    ctz 0x8000000000000000 = 63
    ctz 0x0 = 64
    ctz 0x1 = 0
    ctz 0x7 = 0
    ctz 0x2 = 1
    ctz 0x4 = 2
    ctz 0xc = 2
    ctz 0x12 = 1
    ctz 0xffffffffffffffff = 0
    |}]
;;

let%expect_test "ctz int32" =
  let open Int32 in
  let numbers = List.map numbers ~f:of_int_trunc in
  let f =
    [%template test_op [@kind bits32]]
      (module Int32_u)
      ~op:I.Int32.Unboxed.count_trailing_zeros
      ~op_name:"ctz"
      ~of_t:Int32_u.of_int32
  in
  List.iter ~f (max_value :: min_value :: numbers);
  [%expect
    {|
    ctz 0x7fffffff = 0
    ctz 0x80000000 = 31
    ctz 0x0 = 32
    ctz 0x1 = 0
    ctz 0x7 = 0
    ctz 0x2 = 1
    ctz 0x4 = 2
    ctz 0xc = 2
    ctz 0x12 = 1
    ctz 0xffffffff = 0
    |}]
;;

[%%ifdef JSC_ARCH_SIXTYFOUR]

let%expect_test "ctz nativeint" =
  let open Nativeint in
  let numbers = List.map numbers ~f:of_int in
  let f =
    [%template test_op [@kind word]]
      (module Nativeint_u)
      ~op:I.Nativeint.Unboxed.count_trailing_zeros
      ~op_name:"ctz"
      ~of_t:Nativeint_u.of_nativeint
  in
  List.iter ~f (max_value :: min_value :: numbers);
  [%expect
    {|
    ctz 0x7fffffffffffffff = 0
    ctz 0x8000000000000000 = 63
    ctz 0x0 = 64
    ctz 0x1 = 0
    ctz 0x7 = 0
    ctz 0x2 = 1
    ctz 0x4 = 2
    ctz 0xc = 2
    ctz 0x12 = 1
    ctz 0xffffffffffffffff = 0
    |}]
;;

[%%else]

let%expect_test "ctz nativeint" =
  let open Nativeint in
  let numbers = List.map numbers ~f:of_int in
  let f =
    [%template test_op [@kind word]]
      (module Nativeint_u)
      ~op:I.Nativeint.Unboxed.count_trailing_zeros
      ~op_name:"ctz"
      ~of_t:Nativeint_u.of_nativeint
  in
  List.iter ~f (max_value :: min_value :: numbers);
  [%expect
    {|
    ctz 0x7fff_ffff = 0
    ctz -0x8000_0000 = 31
    ctz 0x0 = 32
    ctz 0x1 = 0
    ctz 0x7 = 0
    ctz 0x2 = 1
    ctz 0x4 = 2
    ctz 0xc = 2
    ctz 0x12 = 1
    ctz -0x1 = 0
    |}]
;;

[%%endif]
