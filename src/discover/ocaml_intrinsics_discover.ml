let () =
  let out = Stdlib.Out_channel.open_text Sys.argv.(1) in
  (match Ocaml_common.Config.config_var "architecture" with
   | Some "amd64" -> Printf.fprintf out "(-march=haswell -mtune=skylake)\n"
   | Some "arm64" -> Printf.fprintf out "(-march=armv8-a+crc)\n"
   | _ -> Printf.fprintf out "()\n");
  Stdlib.Out_channel.close out
;;
