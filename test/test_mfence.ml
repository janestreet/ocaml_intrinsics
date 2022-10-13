module M = Ocaml_intrinsics.Fences

let%expect_test "lfence" =
  M.load_fence ();
  [%expect {||}]
;;

let%expect_test "sfence" =
  M.store_fence ();
  [%expect {||}]
;;

let%expect_test "mfence" =
  M.memory_fence ();
  [%expect {||}]
;;
