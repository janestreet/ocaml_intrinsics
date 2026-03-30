external load_fence : unit -> unit = "caml_load_fence" [@@noalloc] [@@builtin amd64]
external store_fence : unit -> unit = "caml_store_fence" [@@noalloc] [@@builtin amd64]
external memory_fence : unit -> unit = "caml_memory_fence" [@@noalloc] [@@builtin amd64]
