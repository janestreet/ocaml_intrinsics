#include <caml/mlvalues.h>
#include <caml/alloc.h>

#ifdef __x86_64__

uint64_t caml_rdtsc_unboxed() { return __builtin_ia32_rdtsc(); }

uint64_t caml_rdpmc_unboxed(uint32_t c) { return __builtin_ia32_rdpmc(c); }

#elif defined(__aarch64__)

uint64_t caml_rdtsc_unboxed() {
  uint64_t tsc;
  asm volatile("mrs %0, cntvct_el0" : "=r"(tsc));
  return tsc;
}

uint64_t caml_rdpmc_unboxed(__attribute__((unused)) uint32_t c) { return 0; }

#else
#error "Target not supported"
#endif

value caml_rdtsc(void) { return caml_copy_int64(caml_rdtsc_unboxed()); }

value caml_rdpmc(value v1) {
  return caml_copy_int64(caml_rdpmc_unboxed((Int32_val(v1))));
}
