(* These instructions are specific to the x86 memory model,
   hence are only supported on x86_64 targets. *)

(* Load fence: does not execute until all prior instructions have completed locally,
   and no later instruction begins until lfence completes. *)
external load_fence : unit -> unit = "caml_load_fence" [@@noalloc] [@@builtin]

(* Store fence: does not execute until all stores from prior instructions are
   globally visible. *)
external store_fence : unit -> unit = "caml_store_fence" [@@noalloc] [@@builtin]

(* Memory fence: does not execute until all loads and stores from prior instructions have
   completed locally/become globally visible, respectively. *)
external memory_fence : unit -> unit = "caml_memory_fence" [@@noalloc] [@@builtin]
