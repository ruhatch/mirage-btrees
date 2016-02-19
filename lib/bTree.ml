(**

  Simple non-persistent B-Trees

  I would like to formally apologise for the imperative nature of this implementation, but time is permitting

  Pass in block size on creation

  This determines t (minimum degree) as t = (B + 10) / 36

  -8 off the front for pointers and floor it, so t = |_ (B + 2) / 36 _|

  Thus for 512B sectors, get minimum degree of 16 children

  Need to store t somewhere

  Root node is B-Tree and stores t as well as other things

  Each node is then a Cstruct laid out as

  --------------------------------------------------------------------------------------------------------------------------
  | noKeys n | minDegree t | pageSize p | leaf b | child 1 | key 1 | value 1 | child 2 | ... | key n | value n | child n+1 |
  --------------------------------------------------------------------------------------------------------------------------

  Child is a pointer to another Node of the B-Tree (uint64)
  Key is an inode number based on the hash of the filename (uint16)
  Value is a pointer to the actual inode (uint64)

*)

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

module Make (Allocator : ALLOCATOR) (Store : STORE with type pointer = Allocator.pointer) (Node : Node.NODE with type t = Store.page_aligned_buffer and type pointer = Allocator.pointer) = struct

  let (>>=) = Store.bind

  let return = Store.return

  type pointer = Allocator.pointer
  type allocator = Allocator.t
  type store = Store.t
  type error = Store.error
  type 'a io = 'a Store.io
  type node = Node.t
  type key = Node.key
  type value = Node.value

  type t = {
    allocator : allocator;
    store : store;
    mutable root : node;
    mutable rootAddress : pointer;
    minDegree : int;
  }

  (* include S with type pointer = Allocator.pointer
  and type allocator = Allocator.t
  and type store = Store.t
  and type node = Node.t *)

  let ( >>= ) x f = x >>= function
    | `Error e -> return (`Error e)
    | `Ok x -> f x

  let create allocator store pageSize =
    let root = Node.create pageSize in
    Node.setLeaf root true;
    let minDegree = Node.minDegree root in
    begin match Allocator.alloc allocator 1 with
    | [rootAddress] -> return (`Ok rootAddress)
    | _ -> return (`Error (`Unknown ("Wrong number of addresses allocated")))
    end >>= fun rootAddress ->
    Store.write store rootAddress [root] >>= fun () ->
    return (`Ok ({ allocator ; store ; root ; rootAddress ; minDegree }))

  let connect allocator store pageSize rootAddress =
    let root = Node.create pageSize in
    Store.read store rootAddress [root] >>= fun () ->
    let minDegree = Node.minDegree root in
    return (`Ok ({ allocator ; store ; root ; rootAddress ; minDegree }))

  let splitChild t parent parentAddress child childAddress childIndex =
    let newChild = Node.create (Node.pageSize t.root) in
    begin match Allocator.alloc t.allocator 1 with
    | [newChildAddress] -> return (`Ok newChildAddress)
    | _ -> return (`Error (`Unknown ("Wrong number of addresses allocated")))
    end >>= fun newChildAddress ->
    Node.setLeaf newChild (Node.leaf child);
    Node.setNoKeys newChild (t.minDegree - 1);
    for j = 1 to t.minDegree - 1 do
      Node.setKey newChild j (Node.getKey child (j + t.minDegree));
      Node.setValue newChild j (Node.getValue child (j + t.minDegree))
    done;
    if not (Node.leaf child)
    then
      for j = childIndex to t.minDegree do
        Node.setChild newChild j (Node.getChild child (j + t.minDegree))
      done;
    Node.setNoKeys child (t.minDegree - 1);
    for j = Node.noKeys parent + 1 downto childIndex + 1 do
      Node.setChild parent (j + 1) (Node.getChild parent j)
    done;
    Node.setChild parent (childIndex + 1) newChildAddress;
    for j = Node.noKeys parent downto childIndex do
      Node.setKey parent (j + 1) (Node.getKey parent j);
      Node.setValue parent (j + 1) (Node.getValue parent j)
    done;
    Node.setKey parent childIndex (Node.getKey child t.minDegree);
    Node.setValue parent childIndex (Node.getValue child t.minDegree);
    Node.setNoKeys parent (Node.noKeys parent + 1);
    Store.write t.store parentAddress [parent] >>= fun () ->
    Store.write t.store childAddress [child] >>= fun () ->
    Store.write t.store newChildAddress [newChild] >>= fun () ->
    return (`Ok (newChild,newChildAddress))

  let rec insertNonfull t node nodeAddress key value =
    (*Printf.printf "Inserting. Node is leaf? %b\n" (Node.leaf node);*)
    let i = ref (Node.noKeys node) in
    if Node.leaf node
    then (
      while !i >= 1 && key < Node.getKey node !i do
        Node.setKey node (!i + 1) (Node.getKey node !i);
        Node.setValue node (!i + 1) (Node.getValue node !i);
        decr i
      done;
      Node.setKey node (!i + 1) key;
      Node.setValue node (!i + 1) value;
      Node.setNoKeys node (Node.noKeys node + 1);
      Store.write t.store nodeAddress [node]
    ) else (
      while !i >= 1 && key < Node.getKey node !i do
        decr i
      done;
      incr i;
      let child = Node.create (Node.pageSize t.root) in
      let childAddress = Node.getChild node !i in
      Store.read t.store childAddress [child] >>= fun () ->
      if Node.noKeys child = (2 * t.minDegree - 1)
      then (
        splitChild t node nodeAddress child childAddress !i >>= fun (n,na) ->
        if key > Node.getKey node !i
        then insertNonfull t n na key value
        else insertNonfull t child (Node.getChild node !i) key value
      ) else insertNonfull t child (Node.getChild node !i) key value
    )

  let insert t key value =
    if Node.noKeys t.root = (2 * t.minDegree - 1)
    then (
      let s = Node.create (Node.pageSize t.root) in
      begin match Allocator.alloc t.allocator 1 with
      | [sa] -> return (`Ok sa)
      | _ -> return (`Error (`Unknown ("Wrong number of addresses allocated")))
      end >>= fun sa ->
      Node.setLeaf s false;
      Node.setChild s 1 t.rootAddress;
      splitChild t s sa t.root t.rootAddress 1 >>= fun (n, na) ->
      t.root <- s;
      t.rootAddress <- sa;
      insertNonfull t s sa key value
    ) else insertNonfull t t.root t.rootAddress key value

  let rec find t node key =
    (* let keyStrings = Node.printKeys node in
    Printf.printf "Looking for key in list: ";
    List.iter (fun k -> Printf.printf "%s " k) keyStrings;
    Printf.printf "\n"; *)
    let i = ref 1 in
    while !i <= Node.noKeys node && key > Node.getKey node !i do
      incr i;
    done;
    if !i <= Node.noKeys node && key = Node.getKey node !i
    then return (`Ok (Some (Node.getValue node !i)))
    else if Node.leaf node
    then return (`Ok None)
    else (
      let child = Node.create (Node.pageSize t.root) in
      let childAddress = Node.getChild node !i in
      Store.read t.store childAddress [child] >>= fun () ->
      find t child key
    )

end
