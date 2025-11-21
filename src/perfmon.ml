(** The following functions are Intel specific. On other targets, they return 0. *)
external rdtsc : unit -> (int64[@unboxed]) @@ portable = "caml_rdtsc" "caml_rdtsc_unboxed"
[@@noalloc]

(*=The following will be noalloc on 64-bit:
   Stdlib.Int64.to_int (rdtsc ())
   The top bit of the result of rdtsc will be lost.
*)

external rdpmc : int32 -> int64 @@ portable = "caml_rdpmc" "caml_rdpmc_unboxed"
[@@unboxed] [@@noalloc]
