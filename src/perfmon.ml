external rdtsc : unit -> (int64[@unboxed]) @@ portable = "caml_rdtsc" "caml_rdtsc_unboxed"
[@@noalloc] [@@builtin amd64]

external rdpmc : int32 @ local -> int64 @@ portable = "caml_rdpmc" "caml_rdpmc_unboxed"
[@@unboxed] [@@noalloc] [@@builtin amd64]
