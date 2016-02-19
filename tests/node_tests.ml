open Core_kernel.Std
open Alcotest
open Testable
open Generators
open Node

let node_tests =
  [
    "NodeCreate_512_MinDegree14", `Quick,
    (fun () ->
      let node = Node.create 512 in
      check int "" 14 (Node.minDegree node));
    "NodeCreate_512_NoKeys0", `Quick,
    (fun () ->
      let node = Node.create 512 in
      check int "" 0 (Node.noKeys node));
    "NodeCreate_512_PageSize512", `Quick,
    (fun () ->
      let node = Node.create 512 in
      check int "" 512 (Node.pageSize node));
    "NodeSetLeaf_True_LeafTrue", `Quick,
    (fun () ->
      let node = Node.create 512 in
      Node.setLeaf node true;
      check bool "" true (Node.leaf node));
    "NodeSetChild_100_100", `Quick,
    (fun () ->
      let node = Node.create 512 in
      Node.setChild node 1 100L;
      check int64 "" 100L (Node.getChild node 1));
    "NodeSetKey_100_100", `Quick,
    (fun () ->
      let node = Node.create 512 in
      Node.setKey node 1 100;
      check int "" 100 (Node.getKey node 1));
    "NodeSetValue_100_100", `Quick,
    (fun () ->
      let node = Node.create 512 in
      Node.setValue node 1 100L;
      check int64 "" 100L (Node.getValue node 1));
    "NodeSetLeaf_QuickCheck_GetSameLeaf", `Quick,
    (fun () ->
      let node = Node.create 512 in
      Quickcheck.test Quickcheck.Generator.bool
                      (fun leaf ->
                        Node.setLeaf node leaf;
                        check bool "" leaf (Node.leaf node)));
    "NodeSetChild_QuickCheck_GetSamePointer", `Quick,
    (fun () ->
      let node = Node.create 512 in
      let maxDegree = Node.minDegree node * 2 in
      Quickcheck.test (Quickcheck.Generator.tuple2
                         (indexGenerator maxDegree)
                         pointerGenerator)
                      (fun (index, pointer) ->
                        Node.setChild node index pointer;
                        check int64 "" pointer (Node.getChild node index)));
    "NodeSetKey_QuickCheck_GetSameKey", `Quick,
    (fun () ->
      let node = Node.create 512 in
      let maxDegree = Node.minDegree node * 2 in
      Quickcheck.test (Quickcheck.Generator.tuple2
                         (indexGenerator (maxDegree - 1))
                         keyGenerator)
                      (fun (index, key) ->
                        Node.setKey node index key;
                        check int "" key (Node.getKey node index)));
    "NodeSetValue_QuickCheck_GetSameValue", `Quick,
    (fun () ->
      let node = Node.create 512 in
      let maxDegree = Node.minDegree node * 2 in
      Quickcheck.test (Quickcheck.Generator.tuple2
                         (indexGenerator (maxDegree - 1))
                         pointerGenerator)
                      (fun (index, pointer) ->
                        Node.setValue node index pointer;
                        check int64 "" pointer (Node.getValue node index)));

  ]

let () =
  Alcotest.run "Node Tests" [
                 "Node Tests", node_tests
               ]
