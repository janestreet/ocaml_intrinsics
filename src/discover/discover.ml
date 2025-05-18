open Configurator.V1

let prog_aarch64 =
  {|
#ifndef __aarch64__
#error "Not aarch64"
#endif
#include <stdint.h>
#ifdef  __ARM_FEATURE_CRC32
#include <arm_acle.h>
#endif
int main() {
    int64_t i64, d64;
    int32_t i32, d32;
    __crc32cd(i64, d64);
    __crc32cw(i32, d32);
    return 0;
}|}
;;

let prog_x86_64 =
  {|
#ifndef __x86_64__
#error "Not x86_64"
#endif
#include <stdint.h>
#ifdef _MSC_VER
#include <intrin.h>
#else
#include <smmintrin.h>
#endif
int main() {
    int64_t i64, d64;
    int32_t i32, d32;
    _mm_crc32_u64(i64, d64);
    _mm_crc32_u32(i32, d32);
    return 0;
}|}
;;

let () =
  let output = ref "" in
  main
    ~name:"discover"
    ~args:[ "-o", Set_string output, "FILENAME output file" ]
    (fun c ->
      let flags =
        List.filter_map
          (fun (flag, prog) ->
            match c_test c prog ~c_flags:[ flag ] with
            | true -> Some flag
            | false -> None)
          [ "-march=haswell", prog_x86_64; "-march=armv8-a+crc", prog_aarch64 ]
        |> List.sort_uniq String.compare
      in
      Flags.write_sexp !output flags)
;;
