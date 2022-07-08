open Base
open Stdio
module I = Ocaml_intrinsics.Perfmon

let[@cold] work () =
  let min = 0 in
  let max = 100 in
  let e = ref 0 in
  for i = min to max do
    e := !e + i
  done;
  (Sys.opaque_identity !e : int) |> ignore
;;

let%expect_test "rdtsc" =
  let before = I.rdtsc () in
  work ();
  let after = I.rdtsc () in
  let pass =
    (not (Int64.equal before after)) || (Int64.equal before 0L && Int64.equal after 0L)
  in
  printf "%B" pass;
  [%expect {| true |}]
;;

(* Compilation test only, do not run it because input values are not
   valid on all targets and invalid inputs can cause a segfault. *)
let _test_rdpmc () = I.rdpmc 0l
