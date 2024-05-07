## Release v0.17.0

- Split out ocaml_intrinsics_kernel into a separate library that works with javascript.
  It contains most Int* intrinsics, Conditional, Float.max/min.
- Change the C name of `Float.max` and `Float.min` intrinsics.
- Add `Ext_pointer.offset_by_2n` function.
- Add aarch64 equivalent of rdtsc (see
  https://github.com/janestreet/ocaml_intrinsics/pull/7)
- Add `PEXT` and `PDEP` intrinsics
- Simplify the discovery of C optimization flags
