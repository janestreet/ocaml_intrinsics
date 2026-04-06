#include <caml/mlvalues.h>

#ifdef __SSE2__
#include <immintrin.h>

value caml_load_fence(__attribute__((unused)) value unit) {
  _mm_lfence();
  return Val_unit;
}

value caml_store_fence(__attribute__((unused)) value unit) {
  _mm_sfence();
  return Val_unit;
}

value caml_memory_fence(__attribute__((unused)) value unit) {
  _mm_mfence();
  return Val_unit;
}

#elif __aarch64__

value caml_load_fence(__attribute__((unused)) value unit) {
  /* ARM load fence (acquire barrier) - ensures loads before this fence complete
     before any loads after it. Uses inner shareable domain for multi-core sync. */
  __asm__ __volatile__("dmb ishld" ::: "memory");
  return Val_unit;
}

value caml_store_fence(__attribute__((unused)) value unit) {
  /* ARM store fence (release barrier) - ensures stores before this fence complete
     before any stores after it. Uses inner shareable domain for multi-core sync. */
  __asm__ __volatile__("dmb ishst" ::: "memory");
  return Val_unit;
}

value caml_memory_fence(__attribute__((unused)) value unit) {
  /* ARM full memory fence - ensures all memory accesses before this fence complete
     before any memory accesses after it. Uses inner shareable domain for multi-core sync.
   */
  __asm__ __volatile__("dmb ish" ::: "memory");
  return Val_unit;
}

#else
#error "Target not supported"
#endif
