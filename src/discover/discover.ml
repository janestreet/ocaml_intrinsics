open Configurator.V1

let prog_popcnt =
  {|
int main(int argc, char ** argv)
{
  return __builtin_popcount(argc);
}
|}
;;

let prog_lzcnt =
  {|
int main(int argc, char ** argv)
{
  return __builtin_ia32_lzcnt_u64(argc);
}
|}
;;

let prog_tzcnt =
  {|
int main(int argc, char ** argv)
{
  return __builtin_ia32_tzcnt_u64(argc);
}
|}
;;

let prog_crc32 =
  {|
int main(int argc, char ** argv)
{
  return __builtin_ia32_crc32di(argc,argc);
}
|}
;;

let prog_crc32_on_32bit_target =
  {|
int main(int argc, char ** argv)
{
  return __builtin_ia32_crc32si(argc,argc);
}
|}
;;

let prog_prefetchw =
  {|
int main(int argc, char ** argv)
{
  __builtin_prefetch(argv, 1, 3);
  return 0;
}
|}
;;

let prog_prefetchwt1 =
  {|
int main(int argc, char ** argv)
{
  __builtin_prefetch(argv, 1, 2);
  return 0;
}
|}
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
              match c_test c ~c_flags:[ flag ] prog with
              | true -> Some flag
              | false -> None)
           [ "-mpopcnt", prog_popcnt
           ; "-mlzcnt", prog_lzcnt
           ; "-mbmi", prog_tzcnt
           ; "-mcrc32", prog_crc32
           ; "-mcrc32", prog_crc32_on_32bit_target
           ; "-mprfchw", prog_prefetchw
           ; "-mprefetchwt1", prog_prefetchwt1
           ]
       in
       Flags.write_sexp !output flags)
;;
