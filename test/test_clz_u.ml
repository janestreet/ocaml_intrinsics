[%%import "config.h"]

open Base
open Stdio
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
  let test ~op ~op_name ~to_string x ~of_t =
    printf "%s %s = %d\n" op_name (to_string x) (op (of_t x))
  in
  let f =
    test
      ~op:I.Int64.Unboxed.count_leading_zeros
      ~op_name:"clz"
      ~to_string:Hex.to_string_hum
      ~of_t:Int64_u.of_int64
  in
  List.iter ~f numbers;
  [%expect
    {|
    clz 0x0 = 64
    clz 0x1 = 63
    clz 0x7 = 61
    clz 0x7fff_ffff_ffff_ffff = 1
    clz -0x8000_0000_0000_0000 = 0
    clz -0x1 = 0
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
  let test ~op ~op_name ~to_string x ~of_t =
    printf "%s %s = %d\n" op_name (to_string x) (op (of_t x))
  in
  let f =
    test
      ~op:I.Int32.Unboxed.count_leading_zeros
      ~op_name:"clz"
      ~to_string:Hex.to_string_hum
      ~of_t:Int32_u.of_int32
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

[%%ifdef JSC_ARCH_SIXTYFOUR]

let test ~op ~op_name ~to_string ~of_t x =
  printf "%s %s = %d\n" op_name (to_string x) (op (of_t x))
;;

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
    test
      ~op:I.Nativeint.Unboxed.count_leading_zeros
      ~op_name:"clz"
      ~to_string:Hex.to_string_hum
      ~of_t:Nativeint_u.of_nativeint
  in
  List.iter ~f numbers;
  [%expect
    {|
    clz 0x0 = 64
    clz 0x1 = 63
    clz 0x7 = 61
    clz 0x7fff_ffff_ffff_ffff = 1
    clz -0x8000_0000_0000_0000 = 0
    clz -0x1 = 0
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
    test
      ~op:I.Nativeint.Unboxed.count_leading_zeros
      ~op_name:"clz"
      ~to_string:Hex.to_string_hum
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
