open Base
open Stdio
module P = Ocaml_intrinsics.Ext_pointer

module Ext_pointer_to_untagged_int : sig
  type t

  val create : int -> t
  val read : t -> int
  val write : t -> int -> unit
end = struct
  type t = P.t

  external alloc : int -> int = "external_untagged_int_ref"

  let create init_val = P.create (alloc init_val)
  let read t = P.load_untagged_int t
  let write t i = P.store_untagged_int t i
end

module Ext_pointer_to_unboxed_float : sig
  type t

  val create : float -> t
  val read : t -> float
  val write : t -> float -> unit
end = struct
  type t = P.t

  external alloc : float -> int = "external_unboxed_float_ref"

  let create init_val = P.create (alloc init_val)
  let read t = P.load_unboxed_float t
  let write t f = P.store_unboxed_float t f
end

let test_int n =
  let ir = Ext_pointer_to_untagged_int.create n in
  let n' = Ext_pointer_to_untagged_int.read ir in
  printf "ext_pointer int: read %d = %d\n" n n';
  let k = n + 1 in
  Ext_pointer_to_untagged_int.write ir k;
  let k' = Ext_pointer_to_untagged_int.read ir in
  printf "ext_pointer int: read %d = %d\n" k k';
  n', k'
;;

let test_float n =
  let ir = Ext_pointer_to_unboxed_float.create n in
  let n' = Ext_pointer_to_unboxed_float.read ir in
  printf "ext_pointer float: read %f = %f\n" n n';
  let k = n *. 13.0 in
  Ext_pointer_to_unboxed_float.write ir k;
  let k' = Ext_pointer_to_unboxed_float.read ir in
  printf "ext_pointer float: read %f = %f\n" k k';
  n', k'
;;

let%expect_test "ext_pointer int" =
  let numbers = [ 17 ] in
  List.iter ~f:(fun n -> ignore (test_int n : int * int)) numbers;
  [%expect
    {|
    ext_pointer int: read 17 = 17
    ext_pointer int: read 18 = 18
    |}]
;;

let%expect_test "ext_pointer float" =
  let numbers = [ 42.0 ] in
  List.iter ~f:(fun n -> ignore (test_float n : float * float)) numbers;
  [%expect
    {|
    ext_pointer float: read 42.000000 = 42.000000
    ext_pointer float: read 546.000000 = 546.000000
    |}]
;;

include Base_quickcheck.Export

module BI = struct
  include Base.Int

  type t = int [@@deriving quickcheck]
end

module _ = struct
  external alloc : int -> int = "external_immediate_ref"

  let create_immediate n = P.create (alloc n)

  let direct_test_immediate n =
    let ir = create_immediate n in
    let n' = P.Int.unsafe_load_immediate ir in
    printf "native_pointer int: read %d = %d\n" n n';
    let k = n + 1 in
    P.Int.store_immediate ir k;
    let k' = P.Int.unsafe_load_immediate ir in
    printf "native_pointer int: read %d = %d\n" k k';
    n', k'
  ;;

  let%test_unit "native_pointer immediate quickcheck" =
    Base_quickcheck.Test.run_exn (module BI) ~f:(fun n ->
      let expect = n, n + 1 in
      let actual = direct_test_immediate n in
      [%test_result: Int.t * Int.t] ~expect actual)
  ;;

  type t = int [@@deriving quickcheck]
end

module _ = struct
  let%test_unit "ext_pointer int quickcheck" =
    Base_quickcheck.Test.run_exn (module BI) ~f:(fun n ->
      let expect = n, n + 1 in
      let actual = test_int n in
      [%test_result: Int.t * Int.t] ~expect actual)
  ;;

  type t = int [@@deriving quickcheck]
end

module _ = struct
  module BF = struct
    include Base.Float

    type t = float [@@deriving quickcheck]
  end

  let%test_unit "ext_pointer float quickcheck" =
    Base_quickcheck.Test.run_exn (module BF) ~f:(fun n ->
      let expect = n, n *. 13.0 in
      let actual = test_float n in
      [%test_result: Float.t * Float.t] ~expect actual)
  ;;

  type t = int [@@deriving quickcheck]
end

module _ = struct
  external alloc : int64 -> int = "external_unboxed_int64_ref"

  let create_unboxed_int64 n = P.create (alloc n)

  let direct_test_unboxed_int64 n =
    let ir = create_unboxed_int64 n in
    let n' = P.load_unboxed_int64 ir in
    printf "native_pointer float: read %Ld = %Ld\n" n n';
    let k = Int64.(n + 7L) in
    P.store_unboxed_int64 ir k;
    let k' = P.load_unboxed_int64 ir in
    printf "native_pointer float: read %Ld = %Ld\n" k k';
    n', k'
  ;;

  module BInt64 = struct
    include Base.Int64

    type t = int64 [@@deriving quickcheck]
  end

  let%test_unit "native_pointer unboxed int64 quickcheck" =
    Base_quickcheck.Test.run_exn (module BInt64) ~f:(fun n ->
      let expect = n, Int64.(n + 7L) in
      let actual = direct_test_unboxed_int64 n in
      [%test_result: Int64.t * Int64.t] ~expect actual)
  ;;

  type t = int [@@deriving quickcheck]
end

module _ = struct
  external alloc : int32 -> int = "external_unboxed_int32_ref"

  let create_unboxed_int32 n = P.create (alloc n)

  let direct_test_unboxed_int32 n =
    let ir = create_unboxed_int32 n in
    let n' = P.load_unboxed_int32 ir in
    printf "native_pointer float: read %ld = %ld\n" n n';
    let k = Int32.(n + 7l) in
    P.store_unboxed_int32 ir k;
    let k' = P.load_unboxed_int32 ir in
    printf "native_pointer float: read %ld = %ld\n" k k';
    n', k'
  ;;

  module BInt32 = struct
    include Base.Int32

    type t = int32 [@@deriving quickcheck]
  end

  let%test_unit "native_pointer unboxed int32 quickcheck" =
    Base_quickcheck.Test.run_exn (module BInt32) ~f:(fun n ->
      let expect = n, Int32.(n + 7l) in
      let actual = direct_test_unboxed_int32 n in
      [%test_result: Int32.t * Int32.t] ~expect actual)
  ;;

  type t = int [@@deriving quickcheck]
end

module _ = struct
  external alloc : nativeint -> int = "external_unboxed_nativeint_ref"

  let create_unboxed_nativeint n = P.create (alloc n)

  let direct_test_unboxed_nativeint n =
    let ir = create_unboxed_nativeint n in
    let n' = P.load_unboxed_nativeint ir in
    printf "native_pointer float: read %nd = %nd\n" n n';
    let k = Nativeint.(n + 7n) in
    P.store_unboxed_nativeint ir k;
    let k' = P.load_unboxed_nativeint ir in
    printf "native_pointer float: read %nd = %nd\n" k k';
    n', k'
  ;;

  module BNativeint = struct
    include Base.Nativeint

    type t = nativeint [@@deriving quickcheck]
  end

  let%test_unit "native_pointer unboxed nativeint quickcheck" =
    Base_quickcheck.Test.run_exn (module BNativeint) ~f:(fun n ->
      let expect = n, Nativeint.(n + 7n) in
      let actual = direct_test_unboxed_nativeint n in
      [%test_result: Nativeint.t * Nativeint.t] ~expect actual)
  ;;

  type t = int [@@deriving quickcheck]
end

module _ = struct
  external alloc : int -> int = "external_untagged_int_array_ref"

  let create_array n = P.create (alloc n)
  let word_size_in_bytes = Sys.word_size_in_bits / 8
  let offset = word_size_in_bytes / 2
  let advance p word = P.offset_by_2n_bytes p (offset * word)

  let print p n =
    for i = 0 to n - 1 do
      let p' = advance p i in
      printf "%2d:%d " i (P.load_untagged_int p')
    done;
    printf "\n"
  ;;

  let%expect_test "ext_pointer offset_by_2n" =
    printf "ext_pointer offset_by_2n\n";
    let n = 20 in
    let p = create_array n in
    print p n;
    P.store_untagged_int p (-1);
    print p n;
    let k = 5 in
    let p' = advance p k in
    let x = P.load_untagged_int p' in
    (* are we pointing to the correct element? *)
    printf "%d\n" x;
    P.store_untagged_int p' (-1);
    print p n;
    print p' (n - k);
    (* check that negative offsets work correctly *)
    let m = -2 in
    let p'' = advance p' m in
    P.store_untagged_int p'' (-2);
    print p n;
    [%expect
      {|
      ext_pointer offset_by_2n
       0:0  1:1  2:2  3:3  4:4  5:5  6:6  7:7  8:8  9:9 10:10 11:11 12:12 13:13 14:14 15:15 16:16 17:17 18:18 19:19
       0:-1  1:1  2:2  3:3  4:4  5:5  6:6  7:7  8:8  9:9 10:10 11:11 12:12 13:13 14:14 15:15 16:16 17:17 18:18 19:19
      5
       0:-1  1:1  2:2  3:3  4:4  5:-1  6:6  7:7  8:8  9:9 10:10 11:11 12:12 13:13 14:14 15:15 16:16 17:17 18:18 19:19
       0:-1  1:6  2:7  3:8  4:9  5:10  6:11  7:12  8:13  9:14 10:15 11:16 12:17 13:18 14:19
       0:-1  1:1  2:2  3:-2  4:4  5:-1  6:6  7:7  8:8  9:9 10:10 11:11 12:12 13:13 14:14 15:15 16:16 17:17 18:18 19:19
      |}]
  ;;
end
