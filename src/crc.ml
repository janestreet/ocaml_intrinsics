external int_crc
  :  initial:int
  -> data:int
  -> int
  = "caml_sse42_int_untagged_crc_bytecode" "caml_sse42_int_untagged_crc"
[@@noalloc] [@@builtin amd64] [@@untagged] [@@no_effects] [@@no_coeffects]

external int64_crc
  :  initial:(int[@untagged])
  -> data:(int64[@unboxed])
  -> (int[@untagged])
  = "caml_sse42_int64_crc_bytecode" "caml_sse42_int64_crc"
[@@noalloc] [@@builtin amd64] [@@no_effects] [@@no_coeffects]

external unboxed_int64_crc
  :  initial:int64
  -> data:int64
  -> int64
  = "caml_sse42_unboxed_int64_crc_bytecode" "caml_sse42_int64_crc"
[@@noalloc] [@@builtin amd64] [@@unboxed] [@@no_effects] [@@no_coeffects]

let iterated_crc_exn ~initial ~iterations ~data =
  if iterations < 0
  then
    raise
      (Invalid_argument
         (Printf.sprintf
            "iterated_crc: iterations=%d is invalid, must be non-negative value"
            iterations));
  let rec loop i ~acc =
    if i < iterations then loop (i + 1) ~acc:(int_crc ~initial:0 ~data:acc) else acc
  in
  loop 1 ~acc:(int_crc ~initial ~data)
;;
