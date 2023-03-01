open Base
open Stdio
open Bigarray
module EP = Ocaml_intrinsics.Ext_pointer
module NP = Ocaml_intrinsics.Native_pointer
module A_NP = Ocaml_intrinsics.Atomic.Native_pointer
module A_EP = Ocaml_intrinsics.Atomic.Ext_pointer
module A_BS = Ocaml_intrinsics.Atomic.Bigstring

let word = Nativeint.of_int (Sys.word_size_in_bits / 8)

let bigstring_of_string s =
  let a = Array1.create char c_layout (String.length s) in
  for i = 0 to String.length s - 1 do
    a.{i} <- s.[i]
  done;
  a
;;

(* int64, int32, and nativeint are boxed: these point to blocks with a one-word
   header and then the value. This uses 1000 so the pointers don't get unified
   with constants in the tests...yeah this is very undefined behavior *)
let int64_ptr = 1000L
let int32_ptr = 1000l
let nativeint_ptr = 1000n

let%expect_test "native pointer -> untagged int" =
  (* unboxed nativeint is the same width as an immediate int *)
  let np = NP.advance (NP.unsafe_of_value nativeint_ptr) ~bytes:word in
  NP.store_unboxed_nativeint np 0n;
  let v0 = A_NP.fetch_and_add np 5 in
  let v1 = A_NP.fetch_and_sub np 2 in
  let v2 = A_NP.fetch_and_add np 0 in
  let s0 = A_NP.compare_and_swap np ~compare_with:3 ~set_to:0 in
  let s1 = A_NP.compare_and_swap np ~compare_with:3 ~set_to:0 in
  printf "%d %d %d %b %b" v0 v1 v2 s0 s1;
  [%expect {| 0 5 3 true false |}]
;;

let%expect_test "ext pointer -> untagged int" =
  (* unboxed nativeint is the same width as an immediate int *)
  let np = NP.advance (NP.unsafe_of_value nativeint_ptr) ~bytes:word in
  NP.store_unboxed_nativeint np 0n;
  let ni = NP.Expert.to_nativeint np in
  let xp = EP.create (Nativeint.to_int_exn Nativeint.(ni / 2n)) in
  let v0 = A_EP.fetch_and_add xp 5 in
  let v1 = A_EP.fetch_and_sub xp 2 in
  let v2 = A_EP.fetch_and_add xp 0 in
  let s0 = A_EP.compare_and_swap xp ~compare_with:3 ~set_to:0 in
  let s1 = A_EP.compare_and_swap xp ~compare_with:3 ~set_to:0 in
  printf "%d %d %d %b %b" v0 v1 v2 s0 s1;
  [%expect {| 0 5 3 true false |}]
;;

let%expect_test "bigstring with offset -> untagged int" =
  let bigstring = bigstring_of_string (String.make 500 '\x00') in
  let a0 = A_BS.fetch_and_add bigstring ~pos:0 5 in
  let a1 = A_BS.fetch_and_sub bigstring ~pos:0 2 in
  let a2 = A_BS.fetch_and_add bigstring ~pos:0 0 in
  let sa0 = A_BS.compare_and_swap bigstring ~pos:0 ~compare_with:3 ~set_to:0 in
  let sa1 = A_BS.compare_and_swap bigstring ~pos:0 ~compare_with:3 ~set_to:0 in
  printf "%d %d %d %b %b\n" a0 a1 a2 sa0 sa1;
  let b0 = A_BS.fetch_and_add bigstring ~pos:3 5 in
  let b1 = A_BS.fetch_and_sub bigstring ~pos:3 2 in
  let b2 = A_BS.fetch_and_add bigstring ~pos:3 0 in
  let sb0 = A_BS.compare_and_swap bigstring ~pos:3 ~compare_with:3 ~set_to:0 in
  let sb1 = A_BS.compare_and_swap bigstring ~pos:3 ~compare_with:3 ~set_to:0 in
  printf "%d %d %d %b %b\n" b0 b1 b2 sb0 sb1;
  [%expect {|
   0 5 3 true false
   0 5 3 true false |}]
;;

let%expect_test "native pointer -> int64" =
  let np = NP.advance (NP.unsafe_of_value int64_ptr) ~bytes:word in
  NP.store_unboxed_int64 np 0L;
  let v0 = A_NP.fetch_and_add_int64 np 5L in
  let v1 = A_NP.fetch_and_sub_int64 np 2L in
  let v2 = A_NP.fetch_and_add_int64 np 0L in
  let s0 = A_NP.compare_and_swap_int64 np ~compare_with:3L ~set_to:0L in
  let s1 = A_NP.compare_and_swap_int64 np ~compare_with:3L ~set_to:0L in
  printf "%Ld %Ld %Ld %b %b" v0 v1 v2 s0 s1;
  [%expect {| 0 5 3 true false |}]
;;

let%expect_test "ext pointer -> int64" =
  let np = NP.advance (NP.unsafe_of_value int64_ptr) ~bytes:word in
  NP.store_unboxed_int64 np 0L;
  let ni = NP.Expert.to_nativeint np in
  let xp = EP.create (Nativeint.to_int_exn Nativeint.(ni / 2n)) in
  let v0 = A_EP.fetch_and_add_int64 xp 5L in
  let v1 = A_EP.fetch_and_sub_int64 xp 2L in
  let v2 = A_EP.fetch_and_add_int64 xp 0L in
  let s0 = A_EP.compare_and_swap_int64 xp ~compare_with:3L ~set_to:0L in
  let s1 = A_EP.compare_and_swap_int64 xp ~compare_with:3L ~set_to:0L in
  printf "%Ld %Ld %Ld %b %b" v0 v1 v2 s0 s1;
  [%expect {| 0 5 3 true false |}]
;;

let%expect_test "bigstring with offset -> int64" =
  let bigstring = bigstring_of_string (String.make 500 '\x00') in
  let a0 = A_BS.fetch_and_add_int64 bigstring ~pos:0 5L in
  let a1 = A_BS.fetch_and_sub_int64 bigstring ~pos:0 2L in
  let a2 = A_BS.fetch_and_add_int64 bigstring ~pos:0 0L in
  let sa0 = A_BS.compare_and_swap_int64 bigstring ~pos:0 ~compare_with:3L ~set_to:0L in
  let sa1 = A_BS.compare_and_swap_int64 bigstring ~pos:0 ~compare_with:3L ~set_to:0L in
  printf "%Ld %Ld %Ld %b %b\n" a0 a1 a2 sa0 sa1;
  let b0 = A_BS.fetch_and_add_int64 bigstring ~pos:3 5L in
  let b1 = A_BS.fetch_and_sub_int64 bigstring ~pos:3 2L in
  let b2 = A_BS.fetch_and_add_int64 bigstring ~pos:3 0L in
  let sb0 = A_BS.compare_and_swap_int64 bigstring ~pos:3 ~compare_with:3L ~set_to:0L in
  let sb1 = A_BS.compare_and_swap_int64 bigstring ~pos:3 ~compare_with:3L ~set_to:0L in
  printf "%Ld %Ld %Ld %b %b\n" b0 b1 b2 sb0 sb1;
  [%expect {|
    0 5 3 true false
    0 5 3 true false |}]
;;

let%expect_test "native pointer -> int32" =
  let np = NP.advance (NP.unsafe_of_value int32_ptr) ~bytes:word in
  NP.store_unboxed_int32 np 0l;
  let v0 = A_NP.fetch_and_add_int32 np 5l in
  let v1 = A_NP.fetch_and_sub_int32 np 2l in
  let v2 = A_NP.fetch_and_add_int32 np 0l in
  let s0 = A_NP.compare_and_swap_int32 np ~compare_with:3l ~set_to:0l in
  let s1 = A_NP.compare_and_swap_int32 np ~compare_with:3l ~set_to:0l in
  printf "%ld %ld %ld %b %b" v0 v1 v2 s0 s1;
  [%expect {| 0 5 3 true false |}]
;;

let%expect_test "ext pointer -> int32" =
  let np = NP.advance (NP.unsafe_of_value int32_ptr) ~bytes:word in
  NP.store_unboxed_int32 np 0l;
  let ni = NP.Expert.to_nativeint np in
  let xp = EP.create (Nativeint.to_int_exn Nativeint.(ni / 2n)) in
  let v0 = A_EP.fetch_and_add_int32 xp 5l in
  let v1 = A_EP.fetch_and_sub_int32 xp 2l in
  let v2 = A_EP.fetch_and_add_int32 xp 0l in
  let s0 = A_EP.compare_and_swap_int32 xp ~compare_with:3l ~set_to:0l in
  let s1 = A_EP.compare_and_swap_int32 xp ~compare_with:3l ~set_to:0l in
  printf "%ld %ld %ld %b %b" v0 v1 v2 s0 s1;
  [%expect {| 0 5 3 true false |}]
;;

let%expect_test "bigstring with offset -> int32" =
  let bigstring = bigstring_of_string (String.make 500 '\x00') in
  let a0 = A_BS.fetch_and_add_int32 bigstring ~pos:0 5l in
  let a1 = A_BS.fetch_and_sub_int32 bigstring ~pos:0 2l in
  let a2 = A_BS.fetch_and_add_int32 bigstring ~pos:0 0l in
  let sa0 = A_BS.compare_and_swap_int32 bigstring ~pos:0 ~compare_with:3l ~set_to:0l in
  let sa1 = A_BS.compare_and_swap_int32 bigstring ~pos:0 ~compare_with:3l ~set_to:0l in
  printf "%ld %ld %ld %b %b\n" a0 a1 a2 sa0 sa1;
  let b0 = A_BS.fetch_and_add_int32 bigstring ~pos:3 5l in
  let b1 = A_BS.fetch_and_sub_int32 bigstring ~pos:3 2l in
  let b2 = A_BS.fetch_and_add_int32 bigstring ~pos:3 0l in
  let sb0 = A_BS.compare_and_swap_int32 bigstring ~pos:3 ~compare_with:3l ~set_to:0l in
  let sb1 = A_BS.compare_and_swap_int32 bigstring ~pos:3 ~compare_with:3l ~set_to:0l in
  printf "%ld %ld %ld %b %b\n" b0 b1 b2 sb0 sb1;
  [%expect {|
    0 5 3 true false
    0 5 3 true false |}]
;;

let%expect_test "native pointer -> nativeint" =
  let np = NP.advance (NP.unsafe_of_value nativeint_ptr) ~bytes:word in
  NP.store_unboxed_nativeint np 0n;
  let v0 = A_NP.fetch_and_add_nativeint np 5n in
  let v1 = A_NP.fetch_and_sub_nativeint np 2n in
  let v2 = A_NP.fetch_and_add_nativeint np 0n in
  let s0 = A_NP.compare_and_swap_nativeint np ~compare_with:3n ~set_to:0n in
  let s1 = A_NP.compare_and_swap_nativeint np ~compare_with:3n ~set_to:0n in
  printf "%nd %nd %nd %b %b" v0 v1 v2 s0 s1;
  [%expect {| 0 5 3 true false |}]
;;

let%expect_test "ext pointer -> nativeint" =
  let np = NP.advance (NP.unsafe_of_value nativeint_ptr) ~bytes:word in
  NP.store_unboxed_nativeint np 0n;
  let ni = NP.Expert.to_nativeint np in
  let xp = EP.create (Nativeint.to_int_exn Nativeint.(ni / 2n)) in
  let v0 = A_EP.fetch_and_add_nativeint xp 5n in
  let v1 = A_EP.fetch_and_sub_nativeint xp 2n in
  let v2 = A_EP.fetch_and_add_nativeint xp 0n in
  let s0 = A_EP.compare_and_swap_nativeint xp ~compare_with:3n ~set_to:0n in
  let s1 = A_EP.compare_and_swap_nativeint xp ~compare_with:3n ~set_to:0n in
  printf "%nd %nd %nd %b %b" v0 v1 v2 s0 s1;
  [%expect {| 0 5 3 true false |}]
;;

let%expect_test "bigstring with offset -> nativeint" =
  let bigstring = bigstring_of_string (String.make 500 '\x00') in
  let a0 = A_BS.fetch_and_add_nativeint bigstring ~pos:0 5n in
  let a1 = A_BS.fetch_and_sub_nativeint bigstring ~pos:0 2n in
  let a2 = A_BS.fetch_and_add_nativeint bigstring ~pos:0 0n in
  let sa0 =
    A_BS.compare_and_swap_nativeint bigstring ~pos:0 ~compare_with:3n ~set_to:0n
  in
  let sa1 =
    A_BS.compare_and_swap_nativeint bigstring ~pos:0 ~compare_with:3n ~set_to:0n
  in
  printf "%nd %nd %nd %b %b\n" a0 a1 a2 sa0 sa1;
  let b0 = A_BS.fetch_and_add_nativeint bigstring ~pos:3 5n in
  let b1 = A_BS.fetch_and_sub_nativeint bigstring ~pos:3 2n in
  let b2 = A_BS.fetch_and_add_nativeint bigstring ~pos:3 0n in
  let sb0 =
    A_BS.compare_and_swap_nativeint bigstring ~pos:3 ~compare_with:3n ~set_to:0n
  in
  let sb1 =
    A_BS.compare_and_swap_nativeint bigstring ~pos:3 ~compare_with:3n ~set_to:0n
  in
  printf "%nd %nd %nd %b %b\n" b0 b1 b2 sb0 sb1;
  [%expect {|
    0 5 3 true false
    0 5 3 true false |}]
;;

let%expect_test "cas loop codegen" =
  let np = NP.advance (NP.unsafe_of_value nativeint_ptr) ~bytes:word in
  NP.store_unboxed_nativeint np 100n;
  while
    not
      (A_NP.compare_and_swap_nativeint
         np
         ~compare_with:(NP.load_unboxed_nativeint np)
         ~set_to:(-1n))
  do
    printf "cas failed\n"
  done;
  printf "%nd" (NP.load_unboxed_nativeint np);
  [%expect {|
    -1|}]
;;

module _ = struct
  include Base_quickcheck.Export

  module BInt = struct
    include Base.Int

    type t = int [@@deriving quickcheck]
  end

  module BInt64 = struct
    include Base.Int64

    type t = int64 [@@deriving quickcheck]
  end

  module BInt32 = struct
    include Base.Int32

    type t = int32 [@@deriving quickcheck]
  end

  module BNative = struct
    include Base.Nativeint

    type t = nativeint [@@deriving quickcheck]
  end

  let%test_unit "native_pointer -> nativeint quickcheck" =
    let np = NP.advance (NP.unsafe_of_value nativeint_ptr) ~bytes:word in
    Base_quickcheck.Test.run_exn
      (module BNative)
      ~f:(fun n ->
        let fetch = A_NP.fetch_and_add_nativeint np n in
        let expect = Nativeint.( + ) fetch n in
        let actual = NP.load_unboxed_nativeint np in
        [%test_result: Nativeint.t] ~expect actual);
    Base_quickcheck.Test.run_exn
      (module BNative)
      ~f:(fun n ->
        let fetch = A_NP.fetch_and_sub_nativeint np n in
        let expect = Nativeint.( - ) fetch n in
        let actual = NP.load_unboxed_nativeint np in
        [%test_result: Nativeint.t] ~expect actual);
    Base_quickcheck.Test.run_exn
      (module BNative)
      ~f:(fun n ->
        let fetch = NP.load_unboxed_nativeint np in
        let swapped = A_NP.compare_and_swap_nativeint np ~compare_with:fetch ~set_to:n in
        [%test_result: bool] ~expect:true swapped;
        let actual = NP.load_unboxed_nativeint np in
        [%test_result: Nativeint.t] ~expect:n actual)
  ;;

  let%test_unit "native_pointer -> int quickcheck" =
    let np = NP.advance (NP.unsafe_of_value nativeint_ptr) ~bytes:word in
    Base_quickcheck.Test.run_exn
      (module BInt)
      ~f:(fun n ->
        let fetch = A_NP.fetch_and_add np n in
        let expect = fetch + n in
        let actual = NP.load_untagged_int np in
        [%test_result: int] ~expect actual);
    Base_quickcheck.Test.run_exn
      (module BInt)
      ~f:(fun n ->
        let fetch = A_NP.fetch_and_sub np n in
        let expect = fetch - n in
        let actual = NP.load_untagged_int np in
        [%test_result: int] ~expect actual);
    Base_quickcheck.Test.run_exn
      (module BInt)
      ~f:(fun n ->
        let fetch = NP.load_untagged_int np in
        let swapped = A_NP.compare_and_swap np ~compare_with:fetch ~set_to:n in
        [%test_result: bool] ~expect:true swapped;
        let actual = NP.load_untagged_int np in
        [%test_result: int] ~expect:n actual)
  ;;

  let%test_unit "native_pointer -> int64 quickcheck" =
    let np = NP.advance (NP.unsafe_of_value int64_ptr) ~bytes:word in
    Base_quickcheck.Test.run_exn
      (module BInt64)
      ~f:(fun n ->
        let fetch = A_NP.fetch_and_add_int64 np n in
        let expect = Int64.( + ) fetch n in
        let actual = NP.load_unboxed_int64 np in
        [%test_result: Int64.t] ~expect actual);
    Base_quickcheck.Test.run_exn
      (module BInt64)
      ~f:(fun n ->
        let fetch = A_NP.fetch_and_sub_int64 np n in
        let expect = Int64.( - ) fetch n in
        let actual = NP.load_unboxed_int64 np in
        [%test_result: Int64.t] ~expect actual);
    Base_quickcheck.Test.run_exn
      (module BInt64)
      ~f:(fun n ->
        let fetch = NP.load_unboxed_int64 np in
        let swapped = A_NP.compare_and_swap_int64 np ~compare_with:fetch ~set_to:n in
        [%test_result: bool] ~expect:true swapped;
        let actual = NP.load_unboxed_int64 np in
        [%test_result: Int64.t] ~expect:n actual)
  ;;

  let%test_unit "native_pointer -> int32 quickcheck" =
    let np = NP.advance (NP.unsafe_of_value int32_ptr) ~bytes:word in
    Base_quickcheck.Test.run_exn
      (module BInt32)
      ~f:(fun n ->
        let fetch = A_NP.fetch_and_add_int32 np n in
        let expect = Int32.( + ) fetch n in
        let actual = NP.load_unboxed_int32 np in
        [%test_result: Int32.t] ~expect actual);
    Base_quickcheck.Test.run_exn
      (module BInt32)
      ~f:(fun n ->
        let fetch = A_NP.fetch_and_sub_int32 np n in
        let expect = Int32.( - ) fetch n in
        let actual = NP.load_unboxed_int32 np in
        [%test_result: Int32.t] ~expect actual);
    Base_quickcheck.Test.run_exn
      (module BInt32)
      ~f:(fun n ->
        let fetch = NP.load_unboxed_int32 np in
        let swapped = A_NP.compare_and_swap_int32 np ~compare_with:fetch ~set_to:n in
        [%test_result: bool] ~expect:true swapped;
        let actual = NP.load_unboxed_int32 np in
        [%test_result: Int32.t] ~expect:n actual)
  ;;

  let%test_unit "ext_pointer -> nativeint quickcheck" =
    let np = NP.advance (NP.unsafe_of_value nativeint_ptr) ~bytes:word in
    let xp =
      EP.create (Nativeint.to_int_exn Nativeint.(NP.Expert.to_nativeint np / 2n))
    in
    Base_quickcheck.Test.run_exn
      (module BNative)
      ~f:(fun n ->
        let fetch = A_EP.fetch_and_add_nativeint xp n in
        let expect = Nativeint.( + ) fetch n in
        let actual = NP.load_unboxed_nativeint np in
        [%test_result: Nativeint.t] ~expect actual);
    Base_quickcheck.Test.run_exn
      (module BNative)
      ~f:(fun n ->
        let fetch = A_EP.fetch_and_sub_nativeint xp n in
        let expect = Nativeint.( - ) fetch n in
        let actual = NP.load_unboxed_nativeint np in
        [%test_result: Nativeint.t] ~expect actual);
    Base_quickcheck.Test.run_exn
      (module BNative)
      ~f:(fun n ->
        let fetch = NP.load_unboxed_nativeint np in
        let swapped = A_EP.compare_and_swap_nativeint xp ~compare_with:fetch ~set_to:n in
        [%test_result: bool] ~expect:true swapped;
        let actual = NP.load_unboxed_nativeint np in
        [%test_result: Nativeint.t] ~expect:n actual)
  ;;

  let%test_unit "ext_pointer -> int quickcheck" =
    let np = NP.advance (NP.unsafe_of_value nativeint_ptr) ~bytes:word in
    let xp =
      EP.create (Nativeint.to_int_exn Nativeint.(NP.Expert.to_nativeint np / 2n))
    in
    Base_quickcheck.Test.run_exn
      (module BInt)
      ~f:(fun n ->
        let fetch = A_EP.fetch_and_add xp n in
        let expect = fetch + n in
        let actual = NP.load_untagged_int np in
        [%test_result: int] ~expect actual);
    Base_quickcheck.Test.run_exn
      (module BInt)
      ~f:(fun n ->
        let fetch = A_EP.fetch_and_sub xp n in
        let expect = fetch - n in
        let actual = NP.load_untagged_int np in
        [%test_result: int] ~expect actual);
    Base_quickcheck.Test.run_exn
      (module BInt)
      ~f:(fun n ->
        let fetch = NP.load_untagged_int np in
        let swapped = A_EP.compare_and_swap xp ~compare_with:fetch ~set_to:n in
        [%test_result: bool] ~expect:true swapped;
        let actual = NP.load_untagged_int np in
        [%test_result: int] ~expect:n actual)
  ;;

  let%test_unit "ext_pointer -> int64 quickcheck" =
    let np = NP.advance (NP.unsafe_of_value int64_ptr) ~bytes:word in
    let xp =
      EP.create (Nativeint.to_int_exn Nativeint.(NP.Expert.to_nativeint np / 2n))
    in
    Base_quickcheck.Test.run_exn
      (module BInt64)
      ~f:(fun n ->
        let fetch = A_EP.fetch_and_add_int64 xp n in
        let expect = Int64.( + ) fetch n in
        let actual = NP.load_unboxed_int64 np in
        [%test_result: Int64.t] ~expect actual);
    Base_quickcheck.Test.run_exn
      (module BInt64)
      ~f:(fun n ->
        let fetch = A_EP.fetch_and_sub_int64 xp n in
        let expect = Int64.( - ) fetch n in
        let actual = NP.load_unboxed_int64 np in
        [%test_result: Int64.t] ~expect actual);
    Base_quickcheck.Test.run_exn
      (module BInt64)
      ~f:(fun n ->
        let fetch = NP.load_unboxed_int64 np in
        let swapped = A_EP.compare_and_swap_int64 xp ~compare_with:fetch ~set_to:n in
        [%test_result: bool] ~expect:true swapped;
        let actual = NP.load_unboxed_int64 np in
        [%test_result: Int64.t] ~expect:n actual)
  ;;

  let%test_unit "ext_pointer -> int32 quickcheck" =
    let np = NP.advance (NP.unsafe_of_value int32_ptr) ~bytes:word in
    let xp =
      EP.create (Nativeint.to_int_exn Nativeint.(NP.Expert.to_nativeint np / 2n))
    in
    Base_quickcheck.Test.run_exn
      (module BInt32)
      ~f:(fun n ->
        let fetch = A_EP.fetch_and_add_int32 xp n in
        let expect = Int32.( + ) fetch n in
        let actual = NP.load_unboxed_int32 np in
        [%test_result: Int32.t] ~expect actual);
    Base_quickcheck.Test.run_exn
      (module BInt32)
      ~f:(fun n ->
        let fetch = A_EP.fetch_and_sub_int32 xp n in
        let expect = Int32.( - ) fetch n in
        let actual = NP.load_unboxed_int32 np in
        [%test_result: Int32.t] ~expect actual);
    Base_quickcheck.Test.run_exn
      (module BInt32)
      ~f:(fun n ->
        let fetch = NP.load_unboxed_int32 np in
        let swapped = A_EP.compare_and_swap_int32 xp ~compare_with:fetch ~set_to:n in
        [%test_result: bool] ~expect:true swapped;
        let actual = NP.load_unboxed_int32 np in
        [%test_result: Int32.t] ~expect:n actual)
  ;;

  let%test_unit "bigstring -> nativeint quickcheck" =
    let bs = bigstring_of_string (String.make 500 '\x00') in
    let load () =
      if Sys.word_size_in_bits = 32
      then
        Base_bigstring.Int_repr.get_int32_le bs ~pos:0
        |> Int_repr.Int32.to_base_int32
        |> Nativeint.of_int32_exn
      else Base_bigstring.Int_repr.get_int64_le bs ~pos:0 |> Nativeint.of_int64_exn
    in
    Base_quickcheck.Test.run_exn
      (module BNative)
      ~f:(fun n ->
        let fetch = A_BS.fetch_and_add_nativeint bs ~pos:0 n in
        let expect = Nativeint.( + ) fetch n in
        let actual = load () in
        [%test_result: Nativeint.t] ~expect actual);
    Base_quickcheck.Test.run_exn
      (module BNative)
      ~f:(fun n ->
        let fetch = A_BS.fetch_and_sub_nativeint bs ~pos:0 n in
        let expect = Nativeint.( - ) fetch n in
        let actual = load () in
        [%test_result: Nativeint.t] ~expect actual);
    Base_quickcheck.Test.run_exn
      (module BNative)
      ~f:(fun n ->
        let fetch = load () in
        let swapped =
          A_BS.compare_and_swap_nativeint bs ~pos:0 ~compare_with:fetch ~set_to:n
        in
        [%test_result: bool] ~expect:true swapped;
        let actual = load () in
        [%test_result: Nativeint.t] ~expect:n actual)
  ;;

  let%test_unit "bigstring -> int quickcheck" =
    let bs = bigstring_of_string (String.make 500 '\x00') in
    let load () =
      if Sys.word_size_in_bits = 32
      then Base_bigstring.get_int32_le bs ~pos:0
      else Base_bigstring.get_int64_le_trunc bs ~pos:0
    in
    Base_quickcheck.Test.run_exn
      (module BInt)
      ~f:(fun n ->
        let fetch = A_BS.fetch_and_add ~pos:0 bs n in
        let expect = fetch + n in
        let actual = load () in
        [%test_result: int] ~expect actual);
    Base_quickcheck.Test.run_exn
      (module BInt)
      ~f:(fun n ->
        let fetch = A_BS.fetch_and_sub ~pos:0 bs n in
        let expect = fetch - n in
        let actual = load () in
        [%test_result: int] ~expect actual);
    Base_quickcheck.Test.run_exn
      (module BInt)
      ~f:(fun n ->
        let fetch = load () in
        let swapped = A_BS.compare_and_swap ~pos:0 bs ~compare_with:fetch ~set_to:n in
        [%test_result: bool] ~expect:true swapped;
        let actual = load () in
        [%test_result: int] ~expect:n actual)
  ;;

  let%test_unit "bigstring -> int64 quickcheck" =
    let bs = bigstring_of_string (String.make 500 '\x00') in
    let load () = Base_bigstring.Int_repr.get_int64_le bs ~pos:0 in
    Base_quickcheck.Test.run_exn
      (module BInt64)
      ~f:(fun n ->
        let fetch = A_BS.fetch_and_add_int64 ~pos:0 bs n in
        let expect = Int64.( + ) fetch n in
        let actual = load () in
        [%test_result: Int64.t] ~expect actual);
    Base_quickcheck.Test.run_exn
      (module BInt64)
      ~f:(fun n ->
        let fetch = A_BS.fetch_and_sub_int64 ~pos:0 bs n in
        let expect = Int64.( - ) fetch n in
        let actual = load () in
        [%test_result: Int64.t] ~expect actual);
    Base_quickcheck.Test.run_exn
      (module BInt64)
      ~f:(fun n ->
        let fetch = load () in
        let swapped =
          A_BS.compare_and_swap_int64 ~pos:0 bs ~compare_with:fetch ~set_to:n
        in
        [%test_result: bool] ~expect:true swapped;
        let actual = load () in
        [%test_result: Int64.t] ~expect:n actual)
  ;;

  let%test_unit "bigstring -> int32 quickcheck" =
    let bs = bigstring_of_string (String.make 500 '\x00') in
    let load () =
      Base_bigstring.Int_repr.get_int32_le bs ~pos:0 |> Int_repr.Int32.to_base_int32
    in
    Base_quickcheck.Test.run_exn
      (module BInt32)
      ~f:(fun n ->
        let fetch = A_BS.fetch_and_add_int32 ~pos:0 bs n in
        let expect = Int32.( + ) fetch n in
        let actual = load () in
        [%test_result: Int32.t] ~expect actual);
    Base_quickcheck.Test.run_exn
      (module BInt32)
      ~f:(fun n ->
        let fetch = A_BS.fetch_and_sub_int32 ~pos:0 bs n in
        let expect = Int32.( - ) fetch n in
        let actual = load () in
        [%test_result: Int32.t] ~expect actual);
    Base_quickcheck.Test.run_exn
      (module BInt32)
      ~f:(fun n ->
        let fetch = load () in
        let swapped =
          A_BS.compare_and_swap_int32 ~pos:0 bs ~compare_with:fetch ~set_to:n
        in
        [%test_result: bool] ~expect:true swapped;
        let actual = load () in
        [%test_result: Int32.t] ~expect:n actual)
  ;;
end
