open! Base

module%bench Overheads = struct
  (* Using [%bench_fun] to bind the input outside the benchmarked code actually has less
     overhead then using [%bench] naively. *)
  let%bench_fun "int overhead" =
    let n = Sys.opaque_identity (Random.int Int.max_value) in
    fun () -> Fn.id n
  ;;

  let%bench_fun "int64 overhead" =
    let n = Sys.opaque_identity (Random.int64 Int64.max_value) in
    fun () -> Fn.id n
  ;;

  let%bench_fun "int32 overhead" =
    let n = Sys.opaque_identity (Random.int32 Int32.max_value) in
    fun () -> Fn.id n
  ;;

  let%bench_fun "nativeint overhead" =
    let n = Sys.opaque_identity (Random.nativeint Nativeint.max_value) in
    fun () -> Fn.id n
  ;;
end

module%bench [@name "Perfmon.rdtsc"] _ = struct
  (* ocaml_intrinsics library *)
  let%bench_fun "rdtsc" = fun () -> Ocaml_intrinsics.Perfmon.rdtsc ()

  (* ocaml_intrinsics library *)
  let%bench_fun "int_of_rdtsc" =
    fun () -> Int64.to_int_trunc (Ocaml_intrinsics.Perfmon.rdtsc ())
  ;;

  (* Time_stamp_counter *)
  let%bench_fun "Time_stamp_counter.now" = fun () -> Time_stamp_counter.now ()
end

module%bench [@name "Perfmon.rdpmc"] _ = struct
  let cpu_count_hw_cpu_cycles = (1 lsl 30) + 1

  let%bench_fun "rdpmc" =
    fun () -> Ocaml_intrinsics.Perfmon.rdpmc (Int32.of_int_exn cpu_count_hw_cpu_cycles)
  ;;
end

module%bench Crc = struct
  (* ocaml_intrinsics library *)

  let%bench_fun "int_crc" =
    let initial = Sys.opaque_identity (Random.int Int.max_value) in
    let data = Sys.opaque_identity (Random.int Int.max_value) in
    fun () -> Ocaml_intrinsics.Crc.int_crc ~initial ~data
  ;;

  let%bench_fun "int64_crc" =
    let initial = Sys.opaque_identity (Random.int Int.max_value) in
    let data = Sys.opaque_identity (Random.int64 Int64.max_value) in
    fun () -> Ocaml_intrinsics.Crc.int64_crc ~initial ~data
  ;;

  let%bench_fun "iterated_crc_exn" =
    let initial = Sys.opaque_identity (Random.int Int.max_value) in
    let data = Sys.opaque_identity (Random.int Int.max_value) in
    let iterations = Sys.opaque_identity 100 in
    fun () -> Ocaml_intrinsics.Crc.iterated_crc_exn ~initial ~data ~iterations
  ;;
end
