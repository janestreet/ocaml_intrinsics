#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <stdio.h>

static inline value encode(void *ptr)
{
  return (value)((uintnat)ptr | 1);
}

static inline void *alloc_aligned(size_t align_arg, size_t size)
{
  void *ref = NULL;
  size_t align = align_arg > sizeof(void *) ? align_arg : sizeof(void *);
  if (posix_memalign(&ref, align, size)) {
    fprintf(stderr, "Error: cannot allocate aligned %zu bytes\n", size);
    exit(1);
  }
  return ref;
}

static inline intnat *make_untagged_int(value init)
{
  size_t n = sizeof(intnat);
  intnat *ref = alloc_aligned(n,n);
  *ref = (intnat)(Long_val(init));
  return ref;
}

static inline intnat *make_immediate(value init)
{
  size_t n = sizeof(intnat);
  intnat *ref = alloc_aligned(n,n);
  *ref = (intnat)init;
  return ref;
}

static inline double *make_unboxed_float(value init)
{
  size_t n = sizeof(double);
  double* ref = alloc_aligned(n,n);
  *ref = Double_val(init);
  return ref;
}

static inline int64_t *make_unboxed_int64(value init)
{
  size_t n = sizeof(int64_t);
  int64_t* ref = alloc_aligned(n,n);
  *ref = Int64_val(init);
  return ref;
}

static inline int32_t *make_unboxed_int32(value init)
{
  size_t n = sizeof(int32_t);
  int32_t* ref = alloc_aligned(n,n);
  *ref = Int32_val(init);
  return ref;
}

static inline intnat *make_unboxed_nativeint(value init)
{
  size_t n = sizeof(intnat);
  intnat* ref = alloc_aligned(n,n);
  *ref = Nativeint_val(init);
  return ref;
}

static inline intnat *make_untagged_int_array_ref(intnat size)
{
  intnat *res = alloc_aligned(sizeof(intnat), sizeof(intnat)*size);
  for (int i = 0; i < size; i++)
    (*(res+i)) = i;
  return res;
}

CAMLprim value external_untagged_int_ref(value init)
{
  return encode((void *)make_untagged_int(init));
}

CAMLprim value external_immediate_ref(value init)
{
  return encode((void *)make_immediate(init));
}

CAMLprim value external_unboxed_float_ref(value init)
{
  return encode((void *)make_unboxed_float(init));
}

CAMLprim value external_unboxed_int64_ref(value init)
{
  return encode((void *)make_unboxed_int64(init));
}

CAMLprim value external_unboxed_nativeint_ref(value init)
{
  return encode((void *)make_unboxed_nativeint(init));
}

CAMLprim value external_unboxed_int32_ref(value init)
{
  return encode((void *)make_unboxed_int32(init));
}

CAMLprim value external_untagged_int_array_ref(value size)
{
  return encode(make_untagged_int_array_ref(Long_val(size)));
}

CAMLprim value external_untagged_int_ref_as_native_pointer(value init)
{
  return caml_copy_nativeint((uintnat)make_untagged_int(init));
}

CAMLprim value external_immediate_ref_as_native_pointer(value init)
{
  return caml_copy_nativeint((uintnat)make_immediate(init));
}

CAMLprim value external_unboxed_float_ref_as_native_pointer(value init)
{
  return caml_copy_nativeint((uintnat)make_unboxed_float(init));
}

CAMLprim value external_unboxed_int64_ref_as_native_pointer(value init)
{
  return caml_copy_nativeint((uintnat)make_unboxed_int64(init));
}

CAMLprim value external_unboxed_int32_ref_as_native_pointer(value init)
{
  return caml_copy_nativeint((uintnat)make_unboxed_int32(init));
}

CAMLprim value external_unboxed_nativeint_ref_as_native_pointer(value init)
{
  return caml_copy_nativeint((uintnat)make_unboxed_nativeint(init));
}
