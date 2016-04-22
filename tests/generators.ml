open Core_kernel.Std
open Quickcheck.Generator

let addressGenerator sizeSectors =
  let upper_bound = Excl (Int64.to_int_exn sizeSectors) in
  Int.gen_between ~lower_bound:(Incl 0) ~upper_bound
  >>| fun x -> Int64.of_int x

let cstructGenerator size =
  List.gen' ~length:(`Exactly size) Char.gen_alphanum
  >>| fun list -> Cstruct.of_string (String.of_char_list list)

let oblockGenerator sizeSectors sectorSize =
  tuple2 (addressGenerator sizeSectors) (cstructGenerator sectorSize)

let bucketGenerator sizeSectors sectorSize bucketSize =
  List.gen' ~length:(`Exactly bucketSize) ~unique:true (addressGenerator sizeSectors)
  >>= fun addresses ->
  List.gen' ~length:(`Exactly bucketSize) (cstructGenerator sectorSize)
  >>| fun cstructs ->
  List.zip_exn addresses cstructs

let leafGenerator numLeaves =
  let upper_bound = Excl (Int64.to_int_exn numLeaves) in
  Int.gen_between ~lower_bound:(Incl 0) ~upper_bound
  >>| fun x -> Int64.of_int x

let pathGenerator height sizeSectors sectorSize bucketSize =
  List.gen' ~length:(`Exactly (height + 1)) (bucketGenerator sizeSectors sectorSize bucketSize)

let indexGenerator maxDegree =
  Int.gen_between ~lower_bound:(Excl 0) ~upper_bound:(Incl maxDegree)

let pointerGenerator =
  Int.gen_between ~lower_bound:(Incl 0) ~upper_bound:(Excl Int.max_value)
  >>| fun x -> Int64.of_int x

let keyGenerator =
  Int.gen_between ~lower_bound:(Incl 0) ~upper_bound:(Incl 0xFF)
