@@ portable

(** Bring the line of data from memory that contains the specified address.

    The instruction is a hint that is ignored on some targets. Some values or
    [temporal_locality] and [operation] hints are not supported on some targets. *)

(** [temporal locality] is a hint to a location in the cache hierarchy where the prefetch
    data should be placed.

    Higher temporal locality hint means prefetching data closer to the CPU. For example,
    on Intel targets, [High] means prefetch to all levels of cache, and Moderate means
    prefetch to L2 cache and higher, but not L1 cache. *)
type temporal_locality =
  | None
  | Low
  | Moderate
  | High

(** Anticipated operation that the data will used for. In preparation for write, the data
    can be brought into cache in exclusive state, whereas for read, shared state is
    sufficient. *)
type operation =
  | Read
  | Write

(** [value a] prefetches the OCaml value [a]. It should not be used when [a] is an
    immediate: it is safe, but will attempt to prefetch an arbitrary memory address. *)
val value
  : ('a : value_or_null).
  'a -> operation:operation -> temporal_locality:temporal_locality -> unit

(** [value a ~pos] prefetches record field, tuple or array element at position [pos],
    starting from 0. For records, field positions are determined by the order of fields in
    the type definition. See also [value_byte_offset].

    It should not be used when [a] is an immediate: it is safe, but will attempt to
    prefetch an arbitrary memory address. Similarly, there are no bounds checks on [pos]. *)
val value_pos
  : ('a : value_or_null).
  'a -> pos:int -> operation:operation -> temporal_locality:temporal_locality -> unit

(** [value a ~byte_offset] prefetches memory at [byte_offset] from address of OCaml value
    [a].

    It should not be used when [a] is an immediate: it is safe, but will attempt to
    prefetch an arbitrary memory address. Similarly, there are no bounds checks on
    [byte_offset].

    The difference between [value_pos] and [value_byte_offset]:

    [value_pos] calculates the byte offset given [pos] and the target size, which works
    well for most types.

    [value_byte_offset] expects the caller to calculate the byte offset. It should be used
    for [String.t],[Bytes.t], float arrays, and (in the future) unboxed types. *)
val value_byte_offset
  : ('a : value_or_null).
  'a
  -> byte_offset:int
  -> operation:operation
  -> temporal_locality:temporal_locality
  -> unit

val native_pointer
  :  Native_pointer.t
  -> operation:operation
  -> temporal_locality:temporal_locality
  -> unit

val ext_pointer
  :  Ext_pointer.t
  -> operation:operation
  -> temporal_locality:temporal_locality
  -> unit

val bigstring
  :  Bigstring_intf.t
  -> pos:int
  -> operation:operation
  -> temporal_locality:temporal_locality
  -> unit

(** Processor hint that improves performance of spin-wait loops. *)
val pause : unit -> unit

(** Processor hint to demote a cache line from L1/L2 to L3. *)
val cldemote : Native_pointer.t -> unit
