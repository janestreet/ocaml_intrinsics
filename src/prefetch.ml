type temporal_locality =
  | None
  | Low
  | Moderate
  | High

type operation =
  | Read
  | Write

(* The compiler needs to know statically the operation and the temporal locality hints,
   because they correspond to different instructions on amd64 target and different GCC
   builtins in the C stubs.  We have one "external" declaration for each pair of
   operation, locality, and pointer type. Naming convention:
   caml_prefetch_<operation>_<temporal_locality>_<pointer_type>
*)
module Expert = Prefetch_expert

let value p ~operation ~temporal_locality =
  match operation, temporal_locality with
  | Write, High -> Expert.prefetch_write_high p
  | Write, Moderate -> Expert.prefetch_write_moderate p
  | Write, Low -> Expert.prefetch_write_low p
  | Write, None -> Expert.prefetch_write_none p
  | Read, None -> Expert.prefetch_read_none p
  | Read, Low -> Expert.prefetch_read_low p
  | Read, Moderate -> Expert.prefetch_read_moderate p
  | Read, High -> Expert.prefetch_read_high p
;;

let value_byte_offset p ~byte_offset ~operation ~temporal_locality =
  match operation, temporal_locality with
  | Write, High -> Expert.prefetch_write_high_val_offset p ~byte_offset
  | Write, Moderate -> Expert.prefetch_write_moderate_val_offset p ~byte_offset
  | Write, Low -> Expert.prefetch_write_low_val_offset p ~byte_offset
  | Write, None -> Expert.prefetch_write_none_val_offset p ~byte_offset
  | Read, None -> Expert.prefetch_read_none_val_offset p ~byte_offset
  | Read, Low -> Expert.prefetch_read_low_val_offset p ~byte_offset
  | Read, Moderate -> Expert.prefetch_read_moderate_val_offset p ~byte_offset
  | Read, High -> Expert.prefetch_read_high_val_offset p ~byte_offset
;;

let value_pos p ~pos ~operation ~temporal_locality =
  let byte_offset = Sys.word_size / 8 * pos in
  value_byte_offset p ~byte_offset ~operation ~temporal_locality
;;

let native_pointer p ~operation ~temporal_locality =
  match operation, temporal_locality with
  | Write, High -> Expert.prefetch_write_high_native_pointer p
  | Write, Moderate -> Expert.prefetch_write_moderate_native_pointer p
  | Write, Low -> Expert.prefetch_write_low_native_pointer p
  | Write, None -> Expert.prefetch_write_none_native_pointer p
  | Read, None -> Expert.prefetch_read_none_native_pointer p
  | Read, Low -> Expert.prefetch_read_low_native_pointer p
  | Read, Moderate -> Expert.prefetch_read_moderate_native_pointer p
  | Read, High -> Expert.prefetch_read_high_native_pointer p
;;

let ext_pointer p ~operation ~temporal_locality =
  match operation, temporal_locality with
  | Write, High -> Expert.prefetch_write_high_ext_pointer p
  | Write, Moderate -> Expert.prefetch_write_moderate_ext_pointer p
  | Write, Low -> Expert.prefetch_write_low_ext_pointer p
  | Write, None -> Expert.prefetch_write_none_ext_pointer p
  | Read, None -> Expert.prefetch_read_none_ext_pointer p
  | Read, Low -> Expert.prefetch_read_low_ext_pointer p
  | Read, Moderate -> Expert.prefetch_read_moderate_ext_pointer p
  | Read, High -> Expert.prefetch_read_high_ext_pointer p
;;

let bigstring bigstring ~pos ~operation ~temporal_locality =
  match operation, temporal_locality with
  | Write, High -> Expert.prefetch_write_high_bigstring bigstring pos
  | Write, Moderate -> Expert.prefetch_write_moderate_bigstring bigstring pos
  | Write, Low -> Expert.prefetch_write_low_bigstring bigstring pos
  | Write, None -> Expert.prefetch_write_none_bigstring bigstring pos
  | Read, None -> Expert.prefetch_read_none_bigstring bigstring pos
  | Read, Low -> Expert.prefetch_read_low_bigstring bigstring pos
  | Read, Moderate -> Expert.prefetch_read_moderate_bigstring bigstring pos
  | Read, High -> Expert.prefetch_read_high_bigstring bigstring pos
;;

external pause : unit -> unit = "caml_pause_hint" [@@noalloc] [@@builtin]
