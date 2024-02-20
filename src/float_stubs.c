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

int64_t caml_sse2_cast_float64_int64(double x)
{
  return _mm_cvtsd_si64(_mm_set_sd(x));
}

double caml_sse2_float64_min(double x, double y)
{
  return _mm_cvtsd_f64(_mm_min_sd(_mm_set_sd(x), _mm_set_sd(y)));
}

double caml_sse2_float64_max(double x, double y)
{
  return _mm_cvtsd_f64(_mm_max_sd(_mm_set_sd(x), _mm_set_sd(y)));
}

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

#if defined(__GNUC__)
__attribute__((optimize("no-math-errno")))
#endif
int64_t caml_sse2_cast_float64_int64(double x)
{
  return llrint(x);
}

double caml_sse2_float64_min(double x, double y) {
  return x < y ? x : y;
}

double caml_sse2_float64_max(double x, double y) {
  return x > y ? x : y;
}

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

CAMLprim value caml_sse2_cast_float64_int64_bytecode(value x)
{
  return caml_copy_int64(caml_sse2_cast_float64_int64(Double_val(x)));
}

CAMLprim value caml_sse2_float64_min_bytecode(value x, value y)
{
  return caml_copy_double(caml_sse2_float64_min(Double_val(x), Double_val(y)));
}

CAMLprim value caml_sse2_float64_max_bytecode(value x, value y)
{
  return caml_copy_double(caml_sse2_float64_max(Double_val(x), Double_val(y)));
}

CAMLprim value caml_sse41_float64_round_bytecode(value mode, value x)
{
  return caml_copy_double(caml_sse41_float64_round(Int_val(mode), Double_val(x)));
}
