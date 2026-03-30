(* Load fence: does not execute until all prior instructions have completed locally, and
   no later instruction begins until lfence completes. *)
val load_fence : unit -> unit

(* Store fence: does not execute until all stores from prior instructions are globally
   visible. *)
val store_fence : unit -> unit

(* Memory fence: does not execute until all loads and stores from prior instructions have
   completed locally/become globally visible, respectively. *)
val memory_fence : unit -> unit
