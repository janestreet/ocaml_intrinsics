[%%import "config.h"]

open Base
open Stdio

let test ~op ~op_name ~to_string xy =
  let x, y = fst xy, snd xy in
  printf "%s %s %s = %s\n" op_name (to_string x) (to_string y) (to_string (op x y))
;;

let%expect_test "int64 deposit bits" =
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
    test
      ~op:Ocaml_intrinsics.Int64.deposit_bits
      ~op_name:"deposit_bits"
      ~to_string:Hex.to_string_hum
  in
  List.iter ~f (Base.List.Cartesian_product.both numbers numbers);
  [%expect
    {|
    deposit_bits 0x0 0x0 = 0x0
    deposit_bits 0x0 0x1 = 0x0
    deposit_bits 0x0 0x7 = 0x0
    deposit_bits 0x0 0x7fff_ffff_ffff_ffff = 0x0
    deposit_bits 0x0 -0x8000_0000_0000_0000 = 0x0
    deposit_bits 0x0 -0x1 = 0x0
    deposit_bits 0x1 0x0 = 0x0
    deposit_bits 0x1 0x1 = 0x1
    deposit_bits 0x1 0x7 = 0x1
    deposit_bits 0x1 0x7fff_ffff_ffff_ffff = 0x1
    deposit_bits 0x1 -0x8000_0000_0000_0000 = -0x8000_0000_0000_0000
    deposit_bits 0x1 -0x1 = 0x1
    deposit_bits 0x7 0x0 = 0x0
    deposit_bits 0x7 0x1 = 0x1
    deposit_bits 0x7 0x7 = 0x7
    deposit_bits 0x7 0x7fff_ffff_ffff_ffff = 0x7
    deposit_bits 0x7 -0x8000_0000_0000_0000 = -0x8000_0000_0000_0000
    deposit_bits 0x7 -0x1 = 0x7
    deposit_bits 0x7fff_ffff_ffff_ffff 0x0 = 0x0
    deposit_bits 0x7fff_ffff_ffff_ffff 0x1 = 0x1
    deposit_bits 0x7fff_ffff_ffff_ffff 0x7 = 0x7
    deposit_bits 0x7fff_ffff_ffff_ffff 0x7fff_ffff_ffff_ffff = 0x7fff_ffff_ffff_ffff
    deposit_bits 0x7fff_ffff_ffff_ffff -0x8000_0000_0000_0000 = -0x8000_0000_0000_0000
    deposit_bits 0x7fff_ffff_ffff_ffff -0x1 = 0x7fff_ffff_ffff_ffff
    deposit_bits -0x8000_0000_0000_0000 0x0 = 0x0
    deposit_bits -0x8000_0000_0000_0000 0x1 = 0x0
    deposit_bits -0x8000_0000_0000_0000 0x7 = 0x0
    deposit_bits -0x8000_0000_0000_0000 0x7fff_ffff_ffff_ffff = 0x0
    deposit_bits -0x8000_0000_0000_0000 -0x8000_0000_0000_0000 = 0x0
    deposit_bits -0x8000_0000_0000_0000 -0x1 = -0x8000_0000_0000_0000
    deposit_bits -0x1 0x0 = 0x0
    deposit_bits -0x1 0x1 = 0x1
    deposit_bits -0x1 0x7 = 0x7
    deposit_bits -0x1 0x7fff_ffff_ffff_ffff = 0x7fff_ffff_ffff_ffff
    deposit_bits -0x1 -0x8000_0000_0000_0000 = -0x8000_0000_0000_0000
    deposit_bits -0x1 -0x1 = -0x1
    |}]
;;

let%expect_test "int64 extract bits" =
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
    test
      ~op:Ocaml_intrinsics.Int64.extract_bits
      ~op_name:"extract_bits"
      ~to_string:Hex.to_string_hum
  in
  List.iter ~f (Base.List.Cartesian_product.both numbers numbers);
  [%expect
    {|
    extract_bits 0x0 0x0 = 0x0
    extract_bits 0x0 0x1 = 0x0
    extract_bits 0x0 0x7 = 0x0
    extract_bits 0x0 0x7fff_ffff_ffff_ffff = 0x0
    extract_bits 0x0 -0x8000_0000_0000_0000 = 0x0
    extract_bits 0x0 -0x1 = 0x0
    extract_bits 0x1 0x0 = 0x0
    extract_bits 0x1 0x1 = 0x1
    extract_bits 0x1 0x7 = 0x1
    extract_bits 0x1 0x7fff_ffff_ffff_ffff = 0x1
    extract_bits 0x1 -0x8000_0000_0000_0000 = 0x0
    extract_bits 0x1 -0x1 = 0x1
    extract_bits 0x7 0x0 = 0x0
    extract_bits 0x7 0x1 = 0x1
    extract_bits 0x7 0x7 = 0x7
    extract_bits 0x7 0x7fff_ffff_ffff_ffff = 0x7
    extract_bits 0x7 -0x8000_0000_0000_0000 = 0x0
    extract_bits 0x7 -0x1 = 0x7
    extract_bits 0x7fff_ffff_ffff_ffff 0x0 = 0x0
    extract_bits 0x7fff_ffff_ffff_ffff 0x1 = 0x1
    extract_bits 0x7fff_ffff_ffff_ffff 0x7 = 0x7
    extract_bits 0x7fff_ffff_ffff_ffff 0x7fff_ffff_ffff_ffff = 0x7fff_ffff_ffff_ffff
    extract_bits 0x7fff_ffff_ffff_ffff -0x8000_0000_0000_0000 = 0x0
    extract_bits 0x7fff_ffff_ffff_ffff -0x1 = 0x7fff_ffff_ffff_ffff
    extract_bits -0x8000_0000_0000_0000 0x0 = 0x0
    extract_bits -0x8000_0000_0000_0000 0x1 = 0x0
    extract_bits -0x8000_0000_0000_0000 0x7 = 0x0
    extract_bits -0x8000_0000_0000_0000 0x7fff_ffff_ffff_ffff = 0x0
    extract_bits -0x8000_0000_0000_0000 -0x8000_0000_0000_0000 = 0x1
    extract_bits -0x8000_0000_0000_0000 -0x1 = -0x8000_0000_0000_0000
    extract_bits -0x1 0x0 = 0x0
    extract_bits -0x1 0x1 = 0x1
    extract_bits -0x1 0x7 = 0x7
    extract_bits -0x1 0x7fff_ffff_ffff_ffff = 0x7fff_ffff_ffff_ffff
    extract_bits -0x1 -0x8000_0000_0000_0000 = 0x1
    extract_bits -0x1 -0x1 = -0x1
    |}]
;;
