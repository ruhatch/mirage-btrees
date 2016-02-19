module type ALLOCATOR = sig

  type t

  type pointer

  val alloc : t -> int -> pointer list

end

module type STORE = sig

  type t

  type 'a io

  type page_aligned_buffer

  type error = [
    | `Unknown of string
    | `Unimplemented
    | `Is_read_only
    | `Disconnected
  ]

  type pointer

  val bind : 'a io -> ('a -> 'b io) -> 'b io

  val return : 'a -> 'a io

  val read : t -> pointer -> page_aligned_buffer list -> [ `Error of error | `Ok of unit ] io

  val write : t -> pointer -> page_aligned_buffer list -> [ `Error of error | `Ok of unit ] io

end

module type S = sig

  type pointer
  type allocator
  type store
  type error
  type 'a io
  type node
  type key
  type value

  type t = {
    allocator : allocator;
    store : store;
    mutable root : node;
    mutable rootAddress : pointer;
    minDegree : int;
  }

  val create : allocator -> store -> int -> [`Error of error | `Ok of t] io

  val connect : allocator -> store -> int -> pointer -> [`Error of error | `Ok of t] io

  val insert : t -> key -> value -> [`Error of error | `Ok of unit] io

  val find : t -> node -> key -> [`Error of error | `Ok of value option] io

end

module Make
  (Allocator : ALLOCATOR)
  (Store : STORE with type pointer = Allocator.pointer)
  (Node : Node.NODE with type t = Store.page_aligned_buffer and type pointer = Allocator.pointer)
  : S
  with type pointer = Allocator.pointer
  and type allocator = Allocator.t
  and type store = Store.t
  and type error = Store.error
  and type 'a io = 'a Store.io
  and type node = Node.t
  and type key = Node.key
  and type value = Node.value
