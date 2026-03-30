(** On amd64, generates the CRC32Q machine instruction, which requires SSE4.2. *)
val int_crc : initial:int -> data:int -> int

(** On amd64, generates the CRC32Q machine instruction, which requires SSE4.2. *)
val int64_crc : initial:int -> data:int64 -> int

(** On amd64, generates the CRC32Q machine instruction, which requires SSE4.2. *)
val unboxed_int64_crc : initial:int64 -> data:int64 -> int64

(** Accumulates [iterations] of [int_crc]. If [iterations] < 0, raises Invalid_argument. *)
val iterated_crc_exn : initial:int -> iterations:int -> data:int -> int
