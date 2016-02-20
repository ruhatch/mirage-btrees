open Alcotest

let int64 =
  let module M = struct
    type t = int64
    let pp fmt t = Format.fprintf fmt "%Ld" t
    let equal = (=)
  end in
  (module M: TESTABLE with type t = M.t)

let tuple3 (type a) (type b) (type c) (a:a testable) (b:b testable) (c:c testable): (a * b * c) testable =
  let module A = (val a) in
  let module B = (val b) in
  let module C = (val c) in
  (module struct
    type t = a * b * c
    let equal (a1, b1, c1) (a2, b2, c2) = A.equal a1 a2 && B.equal b1 b2 && C.equal c1 c2
    let pp ppf (a, b, c) = A.pp ppf a; Format.pp_print_cut ppf (); B.pp ppf b; Format.pp_print_cut ppf (); C.pp ppf c
  end)

let cstruct =
  let module M = struct
    type t = Cstruct.t
    let pp fmt t = Format.pp_print_string fmt (Cstruct.to_string t)
    let equal = Cstruct.equal
  end in
  (module M: TESTABLE with type t = M.t)

let error =
  let module M = struct
    type t = [
      | `Unknown of string
      | `Unimplemented
      | `Is_read_only
      | `Disconnected
    ]
    let pp fmt t = match t with
      | `Unknown s -> Format.fprintf fmt "Unknown: %s" s
      | `Unimplemented -> Format.pp_print_string fmt "Unimplemented"
      | `Is_read_only -> Format.pp_print_string fmt "Is read only"
      | `Disconnected -> Format.pp_print_string fmt "Disconnected"
    let equal = (=)
  end in
  (module M: TESTABLE with type t = M.t)

let result (type a) error (type b) ok =
  let (module Error: Alcotest.TESTABLE with type t = a) = error in
  let (module Ok: Alcotest.TESTABLE with type t = b) = ok in
  let module M = struct
    type t = [ `Error of a | `Ok of b ]
    let pp fmt t = match t with
      | `Ok x -> Format.fprintf fmt "Ok @[(%a)@]" Ok.pp x
      | `Error x -> Format.fprintf fmt "Error: @[%a@]" Error.pp x
    let equal x y = match x, y with
      | `Ok x, `Ok y -> Ok.equal x y
      | `Error x, `Error y   -> Error.equal x y
      | _ -> false
  end in
  (module M: TESTABLE with type t = M.t)

let oblock = pair int64 cstruct

let bucket = list oblock
