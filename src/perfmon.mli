@@ portable

(** Read the current CPU timestamp counter. *)
val rdtsc : unit -> int64

(** On non-amd64 targets, returns 0. Valid inputs depend on the target, and invalid inputs
    may cause a segfault. *)
val rdpmc : int32 @ local -> int64
