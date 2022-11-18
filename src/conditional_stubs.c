#include "caml/mlvalues.h"

intnat caml_csel_int_untagged(value v_cond, intnat ifso, intnat ifnot)
{
  return (Bool_val(v_cond) ? ifso : ifnot);
}

uint64_t caml_csel_int64_unboxed(value v_cond, uint64_t ifso, uint64_t ifnot)
{
  return (Bool_val(v_cond) ? ifso : ifnot);
}

uint32_t caml_csel_int32_unboxed(value v_cond, uint32_t ifso, uint32_t ifnot)
{
  return (Bool_val(v_cond) ? ifso : ifnot);
}

intnat caml_csel_nativeint_unboxed(value v_cond, intnat ifso, intnat ifnot)
{
  return (Bool_val(v_cond) ? ifso : ifnot);
}

CAMLprim value caml_csel_value(value v_cond, value v_true, value v_false)
{
  return (Bool_val(v_cond) ? v_true : v_false);
}
