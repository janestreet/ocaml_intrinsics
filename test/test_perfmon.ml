open Base
open Stdio
module I = Ocaml_intrinsics.Perfmon

let[@inline never] work () =
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

let%expect_test "rdpmc" =
  let c = 0l in
  let before = I.rdpmc c in
  work ();
  let after = I.rdpmc c in
  let pass =
    (not (Int64.equal before after)) || (Int64.equal before 0L && Int64.equal after 0L)
  in
  printf "%B" pass;
  [%expect {| true |}]
;;
