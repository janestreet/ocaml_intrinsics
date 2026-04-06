open! Base
open! Stdio
module I = Ocaml_intrinsics.Float

let same_bits x y =
  let open Int64 in
  bits_of_float x = bits_of_float y
;;

let args =
  [ Float.infinity
  ; -0x1p+63
  ; -0x1p+62
  ; -3.50
  ; -3.25
  ; -3.00
  ; -2.75
  ; -2.50
  ; -2.25
  ; -2.00
  ; -1.75
  ; -1.50
  ; -1.25
  ; -1.00
  ; -0.75
  ; -0.50
  ; -0.25
  ; -0.00
  ; 0.00
  ; 0.25
  ; 0.50
  ; 0.75
  ; 1.00
  ; 1.25
  ; 1.50
  ; 1.75
  ; 2.00
  ; 2.25
  ; 2.50
  ; 2.75
  ; 3.00
  ; 3.25
  ; 3.50
  ; 0x1.fffffffffffffp+61
  ; 0x1.fffffffffffffp+62
  ; Float.neg_infinity
  ; Float.nan
  ]
;;

let%expect_test "iround_half_to_even" =
  List.iter args ~f:(fun x ->
    (* [iround_half_to_even inf] is unspecified, and differs between amd64 and arm64. *)
    if Float.is_finite x
    then printf "iround_half_to_even %.19g = %Ld\n" x (I.iround_current x));
  [%expect
    {|
    iround_half_to_even -9223372036854775808 = -9223372036854775808
    iround_half_to_even -4611686018427387904 = -4611686018427387904
    iround_half_to_even -3.5 = -4
    iround_half_to_even -3.25 = -3
    iround_half_to_even -3 = -3
    iround_half_to_even -2.75 = -3
    iround_half_to_even -2.5 = -2
    iround_half_to_even -2.25 = -2
    iround_half_to_even -2 = -2
    iround_half_to_even -1.75 = -2
    iround_half_to_even -1.5 = -2
    iround_half_to_even -1.25 = -1
    iround_half_to_even -1 = -1
    iround_half_to_even -0.75 = -1
    iround_half_to_even -0.5 = 0
    iround_half_to_even -0.25 = 0
    iround_half_to_even -0 = 0
    iround_half_to_even 0 = 0
    iround_half_to_even 0.25 = 0
    iround_half_to_even 0.5 = 0
    iround_half_to_even 0.75 = 1
    iround_half_to_even 1 = 1
    iround_half_to_even 1.25 = 1
    iround_half_to_even 1.5 = 2
    iround_half_to_even 1.75 = 2
    iround_half_to_even 2 = 2
    iround_half_to_even 2.25 = 2
    iround_half_to_even 2.5 = 2
    iround_half_to_even 2.75 = 3
    iround_half_to_even 3 = 3
    iround_half_to_even 3.25 = 3
    iround_half_to_even 3.5 = 4
    iround_half_to_even 4611686018427387392 = 4611686018427387392
    iround_half_to_even 9223372036854774784 = 9223372036854774784
    |}]
;;

let%expect_test "round_half_to_even" =
  List.iter args ~f:(fun x ->
    let res = I.round_half_to_even x in
    printf "round_half_to_even %.19g = %.19g\n" x res);
  [%expect
    {|
    round_half_to_even inf = inf
    round_half_to_even -9223372036854775808 = -9223372036854775808
    round_half_to_even -4611686018427387904 = -4611686018427387904
    round_half_to_even -3.5 = -4
    round_half_to_even -3.25 = -3
    round_half_to_even -3 = -3
    round_half_to_even -2.75 = -3
    round_half_to_even -2.5 = -2
    round_half_to_even -2.25 = -2
    round_half_to_even -2 = -2
    round_half_to_even -1.75 = -2
    round_half_to_even -1.5 = -2
    round_half_to_even -1.25 = -1
    round_half_to_even -1 = -1
    round_half_to_even -0.75 = -1
    round_half_to_even -0.5 = -0
    round_half_to_even -0.25 = -0
    round_half_to_even -0 = -0
    round_half_to_even 0 = 0
    round_half_to_even 0.25 = 0
    round_half_to_even 0.5 = 0
    round_half_to_even 0.75 = 1
    round_half_to_even 1 = 1
    round_half_to_even 1.25 = 1
    round_half_to_even 1.5 = 2
    round_half_to_even 1.75 = 2
    round_half_to_even 2 = 2
    round_half_to_even 2.25 = 2
    round_half_to_even 2.5 = 2
    round_half_to_even 2.75 = 3
    round_half_to_even 3 = 3
    round_half_to_even 3.25 = 3
    round_half_to_even 3.5 = 4
    round_half_to_even 4611686018427387392 = 4611686018427387392
    round_half_to_even 9223372036854774784 = 9223372036854774784
    round_half_to_even -inf = -inf
    round_half_to_even nan = nan
    |}]
;;

let%expect_test "bit_of_float" =
  List.iter args ~f:(fun x ->
    let y = Stdlib.Int64.bits_of_float x in
    let z = Stdlib.Int64.float_of_bits y in
    let check = if same_bits x z then "" else " FAIL " in
    printf "bits_of_float %.19g = %Ld -> %.19g%s\n" x y z check);
  [%expect
    {|
    bits_of_float inf = 9218868437227405312 -> inf
    bits_of_float -9223372036854775808 = -4332462841530417152 -> -9223372036854775808
    bits_of_float -4611686018427387904 = -4336966441157787648 -> -4611686018427387904
    bits_of_float -3.5 = -4608308318706860032 -> -3.5
    bits_of_float -3.25 = -4608871268660281344 -> -3.25
    bits_of_float -3 = -4609434218613702656 -> -3
    bits_of_float -2.75 = -4609997168567123968 -> -2.75
    bits_of_float -2.5 = -4610560118520545280 -> -2.5
    bits_of_float -2.25 = -4611123068473966592 -> -2.25
    bits_of_float -2 = -4611686018427387904 -> -2
    bits_of_float -1.75 = -4612811918334230528 -> -1.75
    bits_of_float -1.5 = -4613937818241073152 -> -1.5
    bits_of_float -1.25 = -4615063718147915776 -> -1.25
    bits_of_float -1 = -4616189618054758400 -> -1
    bits_of_float -0.75 = -4618441417868443648 -> -0.75
    bits_of_float -0.5 = -4620693217682128896 -> -0.5
    bits_of_float -0.25 = -4625196817309499392 -> -0.25
    bits_of_float -0 = -9223372036854775808 -> -0
    bits_of_float 0 = 0 -> 0
    bits_of_float 0.25 = 4598175219545276416 -> 0.25
    bits_of_float 0.5 = 4602678819172646912 -> 0.5
    bits_of_float 0.75 = 4604930618986332160 -> 0.75
    bits_of_float 1 = 4607182418800017408 -> 1
    bits_of_float 1.25 = 4608308318706860032 -> 1.25
    bits_of_float 1.5 = 4609434218613702656 -> 1.5
    bits_of_float 1.75 = 4610560118520545280 -> 1.75
    bits_of_float 2 = 4611686018427387904 -> 2
    bits_of_float 2.25 = 4612248968380809216 -> 2.25
    bits_of_float 2.5 = 4612811918334230528 -> 2.5
    bits_of_float 2.75 = 4613374868287651840 -> 2.75
    bits_of_float 3 = 4613937818241073152 -> 3
    bits_of_float 3.25 = 4614500768194494464 -> 3.25
    bits_of_float 3.5 = 4615063718147915776 -> 3.5
    bits_of_float 4611686018427387392 = 4886405595696988159 -> 4611686018427387392
    bits_of_float 9223372036854774784 = 4890909195324358655 -> 9223372036854774784
    bits_of_float -inf = -4503599627370496 -> -inf
    bits_of_float nan = 9221120237041090561 -> nan
    |}]
;;
