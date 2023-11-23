#include "caml/config.h"
#include "caml/mlvalues.h"

#ifdef  __ARM_FEATURE_CRC32
#include <arm_acle.h>

#ifdef ARCH_SIXTYFOUR
static inline uint64_t crc64(uint64_t initial, uint64_t data)
{
  return __crc32cd(initial, data);
}
#else
static inline uint32_t crc32(uint32_t initial, uint32_t data)
{
  return __crc32cw(initial, data);
}
#endif // ARCH_SIXTYFOUR

#elif defined(__SSE4_2__) || defined(_MSC_VER)

#ifdef _MSC_VER
#include <intrin.h>
#else
#include <smmintrin.h>
#endif

#ifdef ARCH_SIXTYFOUR
static inline uint64_t crc64(uint64_t initial, uint64_t data)
{
   return _mm_crc32_u64(initial, data);
}
#else
static inline uint32_t crc32(uint32_t initial, uint32_t data)
{
   return _mm_crc32_u32(initial, data);
}
#endif // ARCH_SIXTYFOUR

#else

#error "Target not supported"
uint64_t crc64(uint64_t initial, uint64_t data);
uint32_t crc32(uint32_t initial, uint32_t data);

#endif

intnat caml_sse42_int_untagged_crc(intnat initial, intnat data)
{
#ifdef ARCH_SIXTYFOUR
  return crc64(initial, (uint64_t) data);
#else
  return crc32(initial, (uint32_t) data);
#endif
}

#ifdef ARCH_SIXTYFOUR
intnat caml_sse42_int64_crc(intnat initial, int64_t data)
{
  return crc64(initial, (uint64_t) data);
}
#else
intnat caml_sse42_int64_crc(__attribute__ ((unused)) intnat initial,
                              __attribute__ ((unused)) int64_t data)
{
  // Instruction crc32q not available on 32-bit platforms
  abort();
}
#endif

CAMLprim value caml_sse42_int_untagged_crc_bytecode(value v_initial, value v_data)
{
  return Val_long(caml_sse42_int_untagged_crc(Long_val(v_initial), Long_val(v_data)));
}

CAMLprim value caml_sse42_int64_crc_bytecode(value v_initial, value v_data)
{
  return Val_long(caml_sse42_int64_crc(Long_val(v_initial), Int64_val(v_data)));
}
