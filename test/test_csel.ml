open Base
open Stdio
module I = Ocaml_intrinsics.Conditional

let%expect_test "csel int" =
  let inputs = [ 0; 1; 4; 6; 5 ] in
  List.iter inputs ~f:(fun a ->
    let expect = if a % 2 = 0 then a else a + 1 in
    let actual = I.select_value (a % 2 = 0) a (a + 1) in
    printf "%d %d\n" expect actual);
  [%expect {|
    0 0
    2 2
    4 4
    6 6
    6 6 |}]
;;

let%expect_test "csel max int value" =
  let inputs = [ 0, 1; 4, 5 ] in
  List.iter inputs ~f:(fun (a, b) ->
    let expect = if a > b then a else b in
    let actual = I.select_value (a > b) a b in
    printf "%d %d\n" expect actual);
  [%expect {|
    1 1
    5 5 |}]
;;

let%expect_test "csel max float value" =
  let inputs = [ 0.5, Float.neg_infinity; 0.0, 0.1; Float.nan, 5.0 ] in
  List.iter inputs ~f:(fun (a, b) ->
    let expect = if Float.(a > b) then a else b in
    let actual = I.select_value Float.(a > b) a b in
    printf "%f %f\n" expect actual);
  [%expect {|
    0.500000 0.500000
    0.100000 0.100000
    5.000000 5.000000
 |}]
;;

let%expect_test "csel max int untagged" =
  let inputs = [ 0, 1; 4, 5 ] in
  List.iter inputs ~f:(fun (a, b) ->
    let expect = if a > b then a else b in
    let actual = I.select_int (a > b) a b in
    printf "%d %d\n" expect actual);
  [%expect {|
    1 1
    5 5 |}]
;;

let%expect_test "csel max int64 unboxed" =
  let inputs = [ 0L, 1L; 4L, 5L; Int64.max_value, Int64.min_value ] in
  List.iter inputs ~f:(fun (a, b) ->
    let expect = if Int64.(a > b) then a else b in
    let actual = I.select_int64 Int64.(a > b) a b in
    printf "%Ld %Ld\n" expect actual);
  [%expect {|
    1 1
    5 5
    9223372036854775807 9223372036854775807 |}]
;;

let%expect_test "csel max int32 unboxed" =
  let inputs = [ 0l, 1l; 4l, 5l; Int32.max_value, Int32.min_value ] in
  List.iter inputs ~f:(fun (a, b) ->
    let expect = if Int32.(a > b) then a else b in
    let actual = I.select_int32 Int32.(a > b) a b in
    printf "%ld %ld\n" expect actual);
  [%expect {|
    1 1
    5 5
    2147483647 2147483647 |}]
;;

[%%import "config.h"]
[%%ifdef JSC_ARCH_SIXTYFOUR]

let%expect_test "csel max nativeint unboxed" =
  let inputs = [ 0n, 1n; 4n, 5n; Nativeint.max_value, Nativeint.min_value ] in
  List.iter inputs ~f:(fun (a, b) ->
    let expect = if Nativeint.(a > b) then a else b in
    let actual = I.select_nativeint Nativeint.(a > b) a b in
    printf "%nd %nd\n" expect actual);
  [%expect {|
    1 1
    5 5
    9223372036854775807 9223372036854775807 |}]
;;

[%%else]

let%expect_test "csel max nativeint unboxed" =
  let inputs = [ 0n, 1n; 4n, 5n; Nativeint.max_value, Nativeint.min_value ] in
  List.iter inputs ~f:(fun (a, b) ->
    let expect = if Nativeint.(a > b) then a else b in
    let actual = I.select_nativeint Nativeint.(a > b) a b in
    printf "%nd %nd\n" expect actual);
  [%expect {|
    1 1
    5 5
    2147483647 2147483647 |}]
;;

[%%endif]

let%expect_test "csel sideffects" =
  let inputs = [ 0, 1; 5, 4 ] in
  List.iter inputs ~f:(fun (a, b) ->
    let expect =
      if a > b
      then (
        printf "hello 0\n";
        a)
      else (
        printf "world 0\n";
        b)
    in
    let actual =
      I.select_value
        (a > b)
        (printf "hello 1\n";
         a)
        (printf "world 1\n";
         b)
    in
    printf "%d %d\n" expect actual);
  [%expect
    {|
    world 0
    world 1
    hello 1
    1 1
    hello 0
    world 1
    hello 1
    5 5 |}]
;;

let%expect_test "min extra moves" =
  (* Currently [min] emits extra moves:
   *
   * actual:
   *
   * camlT__min_266:
   *   movq	%rax, %rdi
   *   movq	%rbx, %rax
   *   cmpq	%rax, %rdi
   *   cmovl	%rdi, %rax
   *   ret
   *
   * [min2] is
   *
   * camlT__min2_273:
   *    cmpq	%rax, %rbx
   *    cmovl	%rbx, %rax
   * 	ret
   * ret *)
  let[@inline never] min (x : int) (y : int) = I.select_value (x < y) x y in
  let[@inline never] min2 (x : int) (y : int) = I.select_value (y < x) y x in
  let inputs = [ 0, 1; 5, 4 ] in
  List.iter inputs ~f:(fun (a, b) ->
    printf "%d " (min a b);
    printf "%d\n" (min2 a b));
  [%expect {|
    0 0
    4 4 |}]
;;

let%expect_test "float deadcode" =
  (* Currently [nop_float] emits extra loads, because there is no dead code elimination
   *    after register allocation:
   *
   *    camlT__nop_float_292:
   *      movsd	(%rbx), %xmm0
   *      movsd	(%rax), %xmm1
   *      ret *)
  let[@inline never] nop_float (x : float) (y : float) : float =
    I.select_value Float.(x > y) x x
  in
  let inputs =
    [ 0.5, Float.neg_infinity; 0.0, 0.1; Float.nan, 5.0; Float.infinity, -0.0 ]
  in
  List.iter inputs ~f:(fun (a, b) -> printf "%f " (nop_float a b));
  [%expect {| 0.500000 0.000000 nan inf |}]
;;
