[%%import "config.h"]

open Base
open Import
module I = Ocaml_intrinsics

let%expect_test "clz int64" =
  let open Int64 in
  let numbers =
    [ 0L (* Int.num_bits *)
    ; 1L (* Int.num_bits - 1 *)
    ; 7L (* Int.num_bits - 3  *)
    ; max_value
    ; min_value
    ; -1L
    ]
  in
  let f =
    [%template test_op [@kind bits64]]
      ~op:I.Int64.Unboxed.count_leading_zeros
      ~op_name:"clz"
      (module Int64_u)
      ~of_t:Int64_u.of_int64
  in
  List.iter ~f numbers;
  [%expect
    {|
    clz 0x0 = 64
    clz 0x1 = 63
    clz 0x7 = 61
    clz 0x7fffffffffffffff = 1
    clz 0x8000000000000000 = 0
    clz 0xffffffffffffffff = 0
    |}]
;;

let%expect_test "clz int32" =
  let open Int32 in
  let numbers =
    [ 0l (* Int.num_bits *)
    ; 1l (* Int.num_bits - 1 *)
    ; 7l (* Int.num_bits - 3  *)
    ; max_value
    ; min_value
    ; -1l
    ]
  in
  let f =
    [%template test_op [@kind bits32]]
      ~op:I.Int32.Unboxed.count_leading_zeros
      ~op_name:"clz"
      (module Int32_u)
      ~of_t:Int32_u.of_int32
  in
  List.iter ~f numbers;
  [%expect
    {|
    clz 0x0 = 32
    clz 0x1 = 31
    clz 0x7 = 29
    clz 0x7fffffff = 1
    clz 0x80000000 = 0
    clz 0xffffffff = 0
    |}]
;;

[%%ifdef JSC_ARCH_SIXTYFOUR]

let%expect_test "clz nativeint" =
  let open Nativeint in
  let numbers =
    [ 0n (* Int.num_bits *)
    ; 1n (* Int.num_bits - 1 *)
    ; 7n (* Int.num_bits - 3  *)
    ; max_value
    ; min_value
    ; -1n
    ]
  in
  let f =
    [%template test_op [@kind word]]
      ~op:I.Nativeint.Unboxed.count_leading_zeros
      ~op_name:"clz"
      (module Nativeint_u)
      ~of_t:Nativeint_u.of_nativeint
  in
  List.iter ~f numbers;
  [%expect
    {|
    clz 0x0 = 64
    clz 0x1 = 63
    clz 0x7 = 61
    clz 0x7fffffffffffffff = 1
    clz 0x8000000000000000 = 0
    clz 0xffffffffffffffff = 0
    |}]
;;

[%%else]

let%expect_test "clz nativeint" =
  let open Nativeint in
  let numbers =
    [ 0n (* Int.num_bits *)
    ; 1n (* Int.num_bits - 1 *)
    ; 7n (* Int.num_bits - 3  *)
    ; max_value
    ; min_value
    ; -1n
    ]
  in
  let f =
    [%template test_op [@kind word]]
      ~op:I.Nativeint.Unboxed.count_leading_zeros
      ~op_name:"clz"
      (module Nativeint_u)
      ~of_t:Nativeint_u.of_nativeint
  in
  List.iter ~f numbers;
  [%expect
    {|
    clz 0x0 = 32
    clz 0x1 = 31
    clz 0x7 = 29
    clz 0x7fff_ffff = 1
    clz -0x8000_0000 = 0
    clz -0x1 = 0
    |}]
;;

[%%endif]
