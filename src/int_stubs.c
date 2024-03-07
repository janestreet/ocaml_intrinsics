#include "caml/config.h"
#include "caml/alloc.h"
#include "caml/misc.h"
#include "caml/mlvalues.h"

/* x86 BMI2 Intrinsics */

#if (defined(__GNUC__) && defined(__BMI2__))
#include <immintrin.h>
#define ARCH_BMI2
#elif (defined(_MSC_VER) && defined(__AVX2__))
// MSVC does not define a BMI2 macro, but AVX2 implies it.
#define ARCH_BMI2
#endif

uint64_t caml_bmi2_int64_deposit_bits(uint64_t a, uint64_t mask)
{
#ifdef ARCH_BMI2
  return _pdep_u64(a, mask);
#else
  /* Platform not supported, but we don't want to prevent the rest of the library from
     building on it. This is a temporary solution. */
  (void)a;
  (void)mask;
  abort();
#endif
}

uint64_t caml_bmi2_int64_extract_bits(uint64_t a, uint64_t mask)
{
#ifdef ARCH_BMI2
  return _pext_u64(a, mask);
#else
  /* Platform not supported, but we don't want to prevent the rest of the library from
     building on it. This is a temporary solution. */
  (void)a;
  (void)mask;
  abort();
#endif
}

CAMLprim value caml_bmi2_int64_deposit_bits_bytecode(value a, value mask) {
  return caml_copy_int64(caml_bmi2_int64_deposit_bits(Int64_val(a), Int64_val(mask)));
}

CAMLprim value caml_bmi2_int64_extract_bits_bytecode(value a, value mask) {
  return caml_copy_int64(caml_bmi2_int64_extract_bits(Int64_val(a), Int64_val(mask)));
}
