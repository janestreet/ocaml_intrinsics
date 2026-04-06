#include <caml/mlvalues.h>
#include <caml/alloc.h>

#ifdef __BMI2__
#include <immintrin.h>

uint64_t caml_bmi2_pdep_int64(uint64_t src, uint64_t mask) {
  return _pdep_u64(src, mask);
}

uint64_t caml_bmi2_pext_int64(uint64_t src, uint64_t mask) {
  return _pext_u64(src, mask);
}

#else

uint64_t caml_bmi2_pdep_int64(uint64_t src, uint64_t mask) {
  uint64_t result = 0;
  for (uint64_t src_bit = 1; mask != 0; src_bit <<= 1) {
    uint64_t lowest_mask_bit = mask & -mask;
    if (src & src_bit) {
      result |= lowest_mask_bit;
    }
    mask &= mask - 1;
  }
  return result;
}

uint64_t caml_bmi2_pext_int64(uint64_t src, uint64_t mask) {
  uint64_t result = 0;
  for (uint64_t result_bit = 1; mask != 0; result_bit <<= 1) {
    uint64_t lowest_mask_bit = mask & -mask;
    if (src & lowest_mask_bit) {
      result |= result_bit;
    }
    mask &= mask - 1;
  }
  return result;
}

#endif

value caml_bmi2_pdep_int64_bytecode(value a, value mask) {
  return caml_copy_int64(caml_bmi2_pdep_int64(Int64_val(a), Int64_val(mask)));
}

value caml_bmi2_pext_int64_bytecode(value a, value mask) {
  return caml_copy_int64(caml_bmi2_pext_int64(Int64_val(a), Int64_val(mask)));
}
