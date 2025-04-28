#include <string.h>

#include <caml/bigarray.h>

#include "ext_pointer.h"

intnat caml_ext_pointer_as_native_pointer (value n)
{
  return (intnat) caml_ext_pointer_decode(n);
}

intnat caml_native_pointer_of_value (value v)
{
  return (intnat) v;
}

value caml_native_pointer_to_value (intnat p)
{
  return (value) p;
}

uintnat caml_native_pointer_of_bigstring(value v_bstr, intnat pos)
{
  return (uintnat)(((char *)Caml_ba_data_val(v_bstr)) + pos);
}

value caml_native_pointer_load_immediate (intnat p)
{
  return *((value *)p);
}

value caml_native_pointer_store_immediate(intnat p, value v)
{
  *((value *)p) = v;
  return Val_unit;
}

unsigned char caml_native_pointer_load_untagged_char(intnat p)
{
  return *((unsigned char *)p);
}

value caml_native_pointer_store_untagged_char(intnat p, unsigned char c)
{
  *((unsigned char *)p) = c;
  return Val_unit;
}

double caml_native_pointer_load_unboxed_float (intnat p)
{
  return *((double *)p);
}

value caml_native_pointer_store_unboxed_float (intnat p, double d)
{
  *((double *)p) = d;
  return Val_unit;
}

int64_t caml_native_pointer_load_unboxed_int64 (intnat p)
{
  return *((int64_t *)p);
}

value caml_native_pointer_store_unboxed_int64 (intnat p, int64_t d)
{
  *((int64_t *)p) = d;
  return Val_unit;
}

int32_t caml_native_pointer_load_unboxed_int32 (intnat p)
{
  return *((int32_t *)p);
}

value caml_native_pointer_store_unboxed_int32 (intnat p, int32_t d)
{
  *((int32_t *)p) = d;
  return Val_unit;
}

intnat caml_native_pointer_load_unboxed_nativeint (intnat p)
{
  return *((intnat *)p);
}

value caml_native_pointer_store_unboxed_nativeint (intnat p, intnat d)
{
  *((intnat *)p) = d;
  return Val_unit;
}

CAMLprim value caml_ext_pointer_as_native_pointer_bytecode (value n)
{
  return caml_copy_nativeint(caml_ext_pointer_as_native_pointer(n));
}

CAMLprim value caml_native_pointer_of_value_bytecode (value v)
{
  return caml_copy_nativeint(caml_native_pointer_of_value(v));
}

CAMLprim value caml_native_pointer_to_value_bytecode (value p)
{
  return caml_native_pointer_to_value(Nativeint_val(p));
}

CAMLprim value caml_native_pointer_of_bigstring_bytecode(value v_bstr, value pos)
{
  return caml_copy_nativeint(caml_native_pointer_of_bigstring(v_bstr, Long_val(pos)));
}

CAMLprim value caml_native_pointer_load_immediate_bytecode (value p)
{
  return caml_native_pointer_load_immediate(Nativeint_val(p));
}

CAMLprim value caml_native_pointer_store_immediate_bytecode (value p, value v)
{
  return caml_native_pointer_store_immediate(Nativeint_val(p), v);
}

CAMLprim value caml_native_pointer_load_unboxed_float_bytecode (value p)
{
  return caml_copy_double(caml_native_pointer_load_unboxed_float(Nativeint_val(p)));
}

CAMLprim value caml_native_pointer_store_unboxed_float_bytecode (value p, value v)
{
  return caml_native_pointer_store_unboxed_float(Nativeint_val(p), Double_val(v));
}

CAMLprim value caml_native_pointer_load_unboxed_int64_bytecode (value p)
{
  return caml_copy_int64(caml_native_pointer_load_unboxed_int64(Nativeint_val(p)));
}

CAMLprim value caml_native_pointer_store_unboxed_int64_bytecode (value p, value v)
{
  return caml_native_pointer_store_unboxed_int64(Nativeint_val(p), Int64_val(v));
}

CAMLprim value caml_native_pointer_load_unboxed_int32_bytecode (value p)
{
  return caml_copy_int32(caml_native_pointer_load_unboxed_int32(Nativeint_val(p)));
}

CAMLprim value caml_native_pointer_store_unboxed_int32_bytecode (value p, value v)
{
  return caml_native_pointer_store_unboxed_int32(Nativeint_val(p), Int32_val(v));
}

CAMLprim value caml_native_pointer_load_unboxed_nativeint_bytecode (value p)
{
  return caml_copy_nativeint(caml_native_pointer_load_unboxed_nativeint(Nativeint_val(p)));
}

CAMLprim value caml_native_pointer_store_unboxed_nativeint_bytecode (value p, value v)
{
  return caml_native_pointer_store_unboxed_nativeint(Nativeint_val(p), Nativeint_val(v));
}

CAMLprim value caml_native_pointer_load_untagged_char_bytecode(value p)
{
  return Val_long(caml_native_pointer_load_untagged_char(Nativeint_val(p)));
}

CAMLprim value caml_native_pointer_store_untagged_char_bytecode(value p, value c)
{
  return caml_native_pointer_store_untagged_char(Nativeint_val(p), Long_val(c));
}

CAMLprim value caml_native_pointer_load_untagged_int_bytecode (value p)
{
  return Val_long(caml_native_pointer_load_unboxed_nativeint(Nativeint_val(p)));
}

CAMLprim value caml_native_pointer_store_untagged_int_bytecode (value p, value v)
{
  return caml_native_pointer_store_unboxed_nativeint(Nativeint_val(p), Long_val(v));
}

CAMLprim void caml_native_pointer_unsafe_blit_to_bigstring(char *p, intnat src_pos,
                                                           value v_bstr, intnat dst_pos,
                                                           intnat len) {
  struct caml_ba_array *ba_dst = Caml_ba_array_val(v_bstr);
  char *src = p + src_pos;
  char *dst = (char *)ba_dst->data + dst_pos;
  memmove(dst, src, len);
}

CAMLprim void caml_native_pointer_unsafe_blit_to_bigstring_bytecode(
    value p, value src_pos, value v_bstr, value dst_pos, value len) {
  caml_native_pointer_unsafe_blit_to_bigstring((char *)Nativeint_val(p),
                                               Long_val(src_pos), v_bstr,
                                               Long_val(dst_pos), Long_val(len));
}

CAMLprim void caml_native_pointer_unsafe_blit(char *src, intnat src_pos, char *dst,
                                              intnat dst_pos, intnat len) {
  memmove(dst + dst_pos, src + src_pos, len);
}

CAMLprim void caml_native_pointer_unsafe_blit_bytecode(value src, value src_pos,
                                                       value dst, value dst_pos,
                                                       value len) {
  caml_native_pointer_unsafe_blit((char *)Nativeint_val(src), Long_val(src_pos),
                                  (char *)Nativeint_val(dst), Long_val(dst_pos),
                                  Long_val(len));
}

CAMLprim void caml_native_pointer_unsafe_memset(char *src, char c, intnat pos,
                                                intnat len) {
  memset(src + pos, c, len);
}

CAMLprim void caml_native_pointer_unsafe_memset_bytecode(value src, value c, value pos,
                                                         value len) {
  caml_native_pointer_unsafe_memset((char *)Nativeint_val(src), Int_val(c), Long_val(pos),
                                    Long_val(len));
}
