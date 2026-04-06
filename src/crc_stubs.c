#include <caml/mlvalues.h>
#include <caml/alloc.h>

#ifdef __SSE4_2__
#include <immintrin.h>

uint64_t caml_sse42_int64_crc(uint64_t initial, uint64_t data) {
  return _mm_crc32_u64(initial, data);
}

#elif defined(__ARM_FEATURE_CRC32)
#include <arm_acle.h>

uint64_t caml_sse42_int64_crc(uint64_t initial, uint64_t data) {
  return __crc32cd(initial, data);
}

#else
#error "Target not supported"
#endif

intnat caml_sse42_int_untagged_crc(intnat initial, intnat data) {
  return caml_sse42_int64_crc(initial, data);
}

value caml_sse42_int_untagged_crc_bytecode(value v_initial, value v_data) {
  return Val_long(caml_sse42_int_untagged_crc(Long_val(v_initial), Long_val(v_data)));
}

value caml_sse42_int64_crc_bytecode(value v_initial, value v_data) {
  return Val_long(caml_sse42_int64_crc(Long_val(v_initial), Int64_val(v_data)));
}

value caml_sse42_unboxed_int64_crc_bytecode(value v_initial, value v_data) {
  return caml_copy_int64(caml_sse42_int64_crc(Int64_val(v_initial), Int64_val(v_data)));
}
