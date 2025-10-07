#include <stdint.h>
#include <assert.h>

#include <caml/alloc.h>
#include <caml/mlvalues.h>


// These also imply _MM_FROUND_NO_EXC
#define ROUND_NEAREST 0x8
#define ROUND_NEG_INF 0x9
#define ROUND_POS_INF 0xA
#define ROUND_ZERO 0xB
#define ROUND_CURRENT 0xC

#if defined(__SSE4_1__) || defined(_MSC_VER)

#ifdef _MSC_VER
#include <intrin.h>
#else // _MSC_VER
#include <smmintrin.h>
#endif // _MSC_VER

double caml_sse41_float64_round(int mode, double x)
{
  __m128d zero = _mm_setzero_pd();

#define CASE(m) \
  case (m): return _mm_cvtsd_f64(_mm_round_sd(zero, _mm_set_sd(x), (m)))

  switch(mode) {
  CASE(ROUND_NEAREST);
  CASE(ROUND_NEG_INF);
  CASE(ROUND_POS_INF);
  CASE(ROUND_ZERO);
  CASE(ROUND_CURRENT);
  default: assert(!"Rounding mode not supported.");
  }

#undef CASE
}

#else // __SSE4_1__ || _MSC_VER

#include <math.h>

#if defined(__GNUC__) && !defined(__llvm__)
__attribute__((optimize("no-math-errno")))
#endif

double caml_sse41_float64_round(int mode, double x) {
  switch(mode) {
  case ROUND_NEG_INF: return floor(x);
  case ROUND_POS_INF: return ceil(x);
  case ROUND_ZERO:    return trunc(x);
  case ROUND_NEAREST: /* Assumes current rounding mode is half-to-even */
  case ROUND_CURRENT: return rint(x);
  default: assert(!"Rounding mode not supported.");
  }
}

#endif // __SSE4_1__

CAMLprim value caml_sse41_float64_round_bytecode(value mode, value x)
{
  return caml_copy_double(caml_sse41_float64_round(Int_val(mode), Double_val(x)));
}
