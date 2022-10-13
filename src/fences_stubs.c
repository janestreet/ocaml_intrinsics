#include "caml/config.h"
#include "caml/mlvalues.h"

#if defined(_MSC_VER)
#warning "Functionality on Windows has not been tested"
#include <intrin.h>
#pragma intrinsic(_mm_lfence)
#pragma intrinsic(_mm_sfence)
#pragma intrinsic(_mm_mfence)
#endif

// SFENCE requires SSE and LFENCE/MFENCE require SSE2

CAMLprim value caml_load_fence (__attribute__ ((unused)) value unit)
{
#if ((defined(__x86_64__) || defined(__i386__)) && defined(__GNUC__) && defined(__SSE2__))
  __builtin_ia32_lfence();
#elif (defined(__x86_64__) && defined(_MSC_VER))
  _mm_lfence();
#else
  /* Platform not supported, but we don't want to prevent the rest of the library from
     building on it. This is a temporary solution. */
  abort();
#endif
  return Val_unit;
}

CAMLprim value caml_store_fence (__attribute__ ((unused)) value unit)
{
#if ((defined(__x86_64__) || defined(__i386__)) && defined(__GNUC__) && defined(__SSE__))
  __builtin_ia32_sfence();
#elif (defined(__x86_64__) && defined(_MSC_VER))
  _mm_sfence();
#else
  /* Platform not supported, but we don't want to prevent the rest of the library from
     building on it. This is a temporary solution. */
  abort();
#endif
  return Val_unit;
}

CAMLprim value caml_memory_fence (__attribute__ ((unused)) value unit)
{
#if ((defined(__x86_64__) || defined(__i386__)) && defined(__GNUC__) && defined(__SSE2__))
  __builtin_ia32_mfence();
#elif (defined(__x86_64__) && defined(_MSC_VER))
  _mm_mfence();
#else
  /* Platform not supported, but we don't want to prevent the rest of the library from
     building on it. This is a temporary solution. */
  abort();
#endif
  return Val_unit;
}
