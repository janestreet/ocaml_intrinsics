external rdtsc : unit -> (int64[@unboxed]) = "caml_rdtsc" "caml_rdtsc_unboxed"
[@@noalloc] [@@builtin amd64]

external rdpmc : int32 -> int64 = "caml_rdpmc" "caml_rdpmc_unboxed"
[@@unboxed] [@@noalloc] [@@builtin amd64]
