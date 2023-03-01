
#include "caml/bigarray.h"
#include "ext_pointer.h"
#include <stdio.h>

static char *bigstring_element_at_pos(value v_bstr, intnat pos)
{
  return ((char *)Caml_ba_data_val(v_bstr)) + pos;
}

#define IMPL_INT(name)                                                                                   \
    intnat caml_native_pointer_##name##_int_untagged(intnat ptr, intnat n)                               \
    {                                                                                                    \
        return __sync_##name((intnat*)ptr, n);                                                           \
    }                                                                                                    \
    CAMLprim value caml_native_pointer_##name##_int_bytecode(value ptr, value n)                         \
    {                                                                                                    \
        return Val_long(                                                                                 \
            caml_native_pointer_##name##_int_untagged(Nativeint_val(ptr), Long_val(n)));                 \
    }                                                                                                    \
    intnat caml_ext_pointer_##name##_int_untagged(value ptr, intnat n)                                   \
    {                                                                                                    \
        intnat* decode = (intnat*)caml_ext_pointer_decode(ptr);                                          \
        return __sync_##name(decode, n);                                                                 \
    }                                                                                                    \
    CAMLprim value caml_ext_pointer_##name##_int_bytecode(value ptr, value n)                            \
    {                                                                                                    \
        return Val_long(                                                                                 \
            caml_ext_pointer_##name##_int_untagged(ptr, Long_val(n)));                                   \
    }                                                                                                    \
    intnat caml_bigstring_##name##_int_untagged(value v_bstr, intnat pos, intnat n)                      \
    {                                                                                                    \
        intnat* decode = (intnat*)bigstring_element_at_pos(v_bstr, pos);                                 \
        return __sync_##name(decode, n);                                                                 \
    }                                                                                                    \
    CAMLprim value caml_bigstring_##name##_int_bytecode(value v_bstr, value pos, value n)                \
    {                                                                                                    \
        return Val_long(                                                                                 \
            caml_bigstring_##name##_int_untagged(v_bstr, Long_val(pos), Long_val(n)));                   \
    }

#define IMPL_INT64(name)                                                                                 \
    int64_t caml_native_pointer_##name##_int64_unboxed(intnat ptr, int64_t n)                            \
    {                                                                                                    \
        return __sync_##name((int64_t*)ptr, n);                                                          \
    }                                                                                                    \
    CAMLprim value caml_native_pointer_##name##_int64_bytecode(value ptr, value n)                       \
    {                                                                                                    \
        return caml_copy_int64(                                                                          \
            caml_native_pointer_##name##_int64_unboxed(Nativeint_val(ptr), Int64_val(n)));               \
    }                                                                                                    \
    int64_t caml_ext_pointer_##name##_int64_unboxed(value ptr, int64_t n)                                \
    {                                                                                                    \
        int64_t* decode = (int64_t*)caml_ext_pointer_decode(ptr);                                        \
        return __sync_##name(decode, n);                                                                 \
    }                                                                                                    \
    CAMLprim value caml_ext_pointer_##name##_int64_bytecode(value ptr, value n)                          \
    {                                                                                                    \
        return caml_copy_int64(                                                                          \
            caml_ext_pointer_##name##_int64_unboxed(ptr, Int64_val(n)));                                 \
    }                                                                                                    \
    int64_t caml_bigstring_##name##_int64_unboxed(value v_bstr, intnat pos, int64_t n)                   \
    {                                                                                                    \
        int64_t* decode = (int64_t*)bigstring_element_at_pos(v_bstr, pos);                               \
        return __sync_##name(decode, n);                                                                 \
    }                                                                                                    \
    CAMLprim value caml_bigstring_##name##_int64_bytecode(value v_bstr, value pos, value n)              \
    {                                                                                                    \
        return caml_copy_int64(                                                                          \
            caml_bigstring_##name##_int64_unboxed(v_bstr, Long_val(pos), Int64_val(n)));                 \
    }

#define IMPL_INT32(name)                                                                                 \
    int32_t caml_native_pointer_##name##_int32_unboxed(intnat ptr, int32_t n)                            \
    {                                                                                                    \
        return __sync_##name((int32_t*)ptr, n);                                                          \
    }                                                                                                    \
    CAMLprim value caml_native_pointer_##name##_int32_bytecode(value ptr, value n)                       \
    {                                                                                                    \
        return caml_copy_int32(                                                                          \
            caml_native_pointer_##name##_int32_unboxed(Nativeint_val(ptr), Int32_val(n)));               \
    }                                                                                                    \
    int32_t caml_ext_pointer_##name##_int32_unboxed(value ptr, int32_t n)                                \
    {                                                                                                    \
        int32_t* decode = (int32_t*)caml_ext_pointer_decode(ptr);                                        \
        return __sync_##name(decode, n);                                                                 \
    }                                                                                                    \
    CAMLprim value caml_ext_pointer_##name##_int32_bytecode(value ptr, value n)                          \
    {                                                                                                    \
        return caml_copy_int32(                                                                          \
            caml_ext_pointer_##name##_int32_unboxed(ptr, Int32_val(n)));                                 \
    }                                                                                                    \
    int32_t caml_bigstring_##name##_int32_unboxed(value v_bstr, intnat pos, int32_t n)                   \
    {                                                                                                    \
        int32_t* decode = (int32_t*)bigstring_element_at_pos(v_bstr, pos);                               \
        return __sync_##name(decode, n);                                                                 \
    }                                                                                                    \
    CAMLprim value caml_bigstring_##name##_int32_bytecode(value v_bstr, value pos, value n)              \
    {                                                                                                    \
        return caml_copy_int32(                                                                          \
            caml_bigstring_##name##_int32_unboxed(v_bstr, Long_val(pos), Int32_val(n)));                 \
    }

#define IMPL_NATIVEINT(name)                                                                             \
    intnat caml_native_pointer_##name##_nativeint_unboxed(intnat ptr, intnat n)                          \
    {                                                                                                    \
        return __sync_##name((intnat*)ptr, n);                                                           \
    }                                                                                                    \
    CAMLprim value caml_native_pointer_##name##_nativeint_bytecode(value ptr, value n)                   \
    {                                                                                                    \
        return caml_copy_nativeint(                                                                      \
            caml_native_pointer_##name##_nativeint_unboxed(Nativeint_val(ptr), Nativeint_val(n)));       \
    }                                                                                                    \
    intnat caml_ext_pointer_##name##_nativeint_unboxed(value ptr, intnat n)                              \
    {                                                                                                    \
        intnat* decode = (intnat*)caml_ext_pointer_decode(ptr);                                          \
        return __sync_##name(decode, n);                                                                 \
    }                                                                                                    \
    CAMLprim value caml_ext_pointer_##name##_nativeint_bytecode(value ptr, value n)                      \
    {                                                                                                    \
        return caml_copy_nativeint(                                                                      \
            caml_ext_pointer_##name##_nativeint_unboxed(ptr, Nativeint_val(n)));                         \
    }                                                                                                    \
    intnat caml_bigstring_##name##_nativeint_unboxed(value v_bstr, intnat pos, intnat n)                 \
    {                                                                                                    \
        intnat* decode = (intnat*)bigstring_element_at_pos(v_bstr, pos);                                 \
        return __sync_##name(decode, n);                                                                 \
    }                                                                                                    \
    CAMLprim value caml_bigstring_##name##_nativeint_bytecode(value v_bstr, value pos, value n)          \
    {                                                                                                    \
        return caml_copy_nativeint(                                                                      \
            caml_bigstring_##name##_nativeint_unboxed(v_bstr, Long_val(pos), Nativeint_val(n)));         \
    }

/* Arithmetic */

IMPL_INT(fetch_and_add)
IMPL_INT(fetch_and_sub)

IMPL_INT64(fetch_and_add)
IMPL_INT64(fetch_and_sub)

IMPL_INT32(fetch_and_add)
IMPL_INT32(fetch_and_sub)

IMPL_NATIVEINT(fetch_and_add)
IMPL_NATIVEINT(fetch_and_sub)

/* Compare and Swap */

CAMLprim value caml_native_pointer_compare_and_swap_int_untagged(intnat ptr, intnat compare, intnat swap) {
    return Val_bool(__sync_bool_compare_and_swap((intnat*)ptr, compare, swap));
}
CAMLprim value caml_native_pointer_compare_and_swap_int_bytecode(value ptr, value compare, value swap) {
    return caml_native_pointer_compare_and_swap_int_untagged(
                    Nativeint_val(ptr), Long_val(compare), Long_val(swap));
}

CAMLprim value caml_native_pointer_compare_and_swap_int64_unboxed(intnat ptr, int64_t compare, int64_t swap) {
    return Val_bool(__sync_bool_compare_and_swap((int64_t*)ptr, compare, swap));
}
CAMLprim value caml_native_pointer_compare_and_swap_int64_bytecode(value ptr, value compare, value swap) {
    return caml_native_pointer_compare_and_swap_int64_unboxed(
                    Nativeint_val(ptr), Int64_val(compare), Int64_val(swap));
}

CAMLprim value caml_native_pointer_compare_and_swap_int32_unboxed(intnat ptr, int32_t compare, int32_t swap) {
    return Val_bool(__sync_bool_compare_and_swap((int32_t*)ptr, compare, swap));
}
CAMLprim value caml_native_pointer_compare_and_swap_int32_bytecode(value ptr, value compare, value swap) {
    return caml_native_pointer_compare_and_swap_int32_unboxed(
                    Nativeint_val(ptr), Int32_val(compare), Int32_val(swap));
}

CAMLprim value caml_native_pointer_compare_and_swap_nativeint_unboxed(intnat ptr, intnat compare, intnat swap) {
    return Val_bool(__sync_bool_compare_and_swap((intnat*)ptr, compare, swap));
}
CAMLprim value caml_native_pointer_compare_and_swap_nativeint_bytecode(value ptr, value compare, value swap) {
    return caml_native_pointer_compare_and_swap_nativeint_unboxed(
                    Nativeint_val(ptr), Nativeint_val(compare), Nativeint_val(swap));
}


CAMLprim value caml_ext_pointer_compare_and_swap_int_untagged(value ptr, intnat compare, intnat swap) {
    intnat* decode = (intnat*)caml_ext_pointer_decode(ptr);
    return Val_bool(__sync_bool_compare_and_swap(decode, compare, swap));
}
CAMLprim value caml_ext_pointer_compare_and_swap_int_bytecode(value ptr, value compare, value swap) {
    return caml_ext_pointer_compare_and_swap_int_untagged(
                        ptr, Long_val(compare), Long_val(swap));
}

CAMLprim value caml_ext_pointer_compare_and_swap_int64_unboxed(value ptr, int64_t compare, int64_t swap) {
    int64_t* decode = (int64_t*)caml_ext_pointer_decode(ptr);
    return Val_bool(__sync_bool_compare_and_swap(decode, compare, swap));
}
CAMLprim value caml_ext_pointer_compare_and_swap_int64_bytecode(value ptr, value compare, value swap) {
    return caml_ext_pointer_compare_and_swap_int64_unboxed(
                        ptr, Int64_val(compare), Int64_val(swap));
}

CAMLprim value caml_ext_pointer_compare_and_swap_int32_unboxed(value ptr, int32_t compare, int32_t swap) {
    int32_t* decode = (int32_t*)caml_ext_pointer_decode(ptr);
    return Val_bool(__sync_bool_compare_and_swap(decode, compare, swap));
}
CAMLprim value caml_ext_pointer_compare_and_swap_int32_bytecode(value ptr, value compare, value swap) {
    return caml_ext_pointer_compare_and_swap_int32_unboxed(
                    ptr, Int32_val(compare), Int32_val(swap));
}

CAMLprim value caml_ext_pointer_compare_and_swap_nativeint_unboxed(value ptr, intnat compare, intnat swap) {
    intnat* decode = (intnat*)caml_ext_pointer_decode(ptr);
    return Val_bool(__sync_bool_compare_and_swap(decode, compare, swap));
}
CAMLprim value caml_ext_pointer_compare_and_swap_nativeint_bytecode(value ptr, value compare, value swap) {
    return caml_ext_pointer_compare_and_swap_nativeint_unboxed(
                    ptr, Nativeint_val(compare), Nativeint_val(swap));
}


CAMLprim value caml_bigstring_compare_and_swap_int_untagged(value v_bstr, intnat pos, intnat compare, intnat swap) {
    intnat* decode = (intnat*)bigstring_element_at_pos(v_bstr, pos);
    return Val_bool(__sync_bool_compare_and_swap(decode, compare, swap));
}
CAMLprim value caml_bigstring_compare_and_swap_int_bytecode(value v_bstr, value pos, value compare, value swap) {
    return caml_bigstring_compare_and_swap_int_untagged(
                    v_bstr, Long_val(pos), Long_val(compare), Long_val(swap));
}

CAMLprim value caml_bigstring_compare_and_swap_int64_unboxed(value v_bstr, intnat pos, int64_t compare, int64_t swap) {
    int64_t* decode = (int64_t*)bigstring_element_at_pos(v_bstr, pos);
    return Val_bool(__sync_bool_compare_and_swap(decode, compare, swap));
}
CAMLprim value caml_bigstring_compare_and_swap_int64_bytecode(value v_bstr, value pos, value compare, value swap) {
    return caml_bigstring_compare_and_swap_int64_unboxed(
                    v_bstr, Long_val(pos), Int64_val(compare), Int64_val(swap));
}

CAMLprim value caml_bigstring_compare_and_swap_int32_unboxed(value v_bstr, intnat pos, int32_t compare, int32_t swap) {
    int32_t* decode = (int32_t*)bigstring_element_at_pos(v_bstr, pos);
    return Val_bool(__sync_bool_compare_and_swap(decode, compare, swap));
}
CAMLprim value caml_bigstring_compare_and_swap_int32_bytecode(value v_bstr, value pos, value compare, value swap) {
    return caml_bigstring_compare_and_swap_int32_unboxed(
                    v_bstr, Long_val(pos), Int32_val(compare), Int32_val(swap));
}

CAMLprim value caml_bigstring_compare_and_swap_nativeint_unboxed(value v_bstr, intnat pos, intnat compare, intnat swap) {
    intnat* decode = (intnat*)bigstring_element_at_pos(v_bstr, pos);
    return Val_bool(__sync_bool_compare_and_swap(decode, compare, swap));
}
CAMLprim value caml_bigstring_compare_and_swap_nativeint_bytecode(value v_bstr, value pos, value compare, value swap) {
    return caml_bigstring_compare_and_swap_nativeint_unboxed(
                    v_bstr, Long_val(pos), Nativeint_val(compare), Nativeint_val(swap));
}
