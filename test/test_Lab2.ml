open Lab2
module H = Hashmap

(* ========== Hash Functions ========== *)

let int_hash = abs

let string_hash s =
  let hash = ref 0 in
  String.iter (fun c -> hash := ((!hash * 31) + Char.code c) mod max_int) s;
  !hash

let test_string_keys () =
  let m =
    H.empty ~hash:string_hash |> H.add "apple" 1 |> H.add "banana" 2
    |> H.add "cherry" 3
  in
  Alcotest.(check int) "string map size" 3 (H.size m);
  Alcotest.(check (option int)) "find apple" (Some 1) (H.find_opt "apple" m);
  Alcotest.(check (option int)) "find banana" (Some 2) (H.find_opt "banana" m);
  Alcotest.(check (option int)) "find cherry" (Some 3) (H.find_opt "cherry" m)

(* ========== Unit Tests ========== *)

let test_empty () =
  let m = H.empty ~hash:int_hash in
  Alcotest.(check int) "empty size" 0 (H.size m);
  Alcotest.(check (option int)) "find in empty" None (H.find_opt 1 m);
  Alcotest.(check bool) "mem in empty" false (H.mem 1 m)

let test_add_single () =
  let m = H.empty ~hash:int_hash |> H.add 1 100 in
  Alcotest.(check int) "size after add" 1 (H.size m);
  Alcotest.(check (option int)) "find added" (Some 100) (H.find_opt 1 m);
  Alcotest.(check bool) "mem added" true (H.mem 1 m)

let test_add_multiple () =
  let m = H.empty ~hash:int_hash |> H.add 1 10 |> H.add 2 20 |> H.add 3 30 in
  Alcotest.(check int) "size after adds" 3 (H.size m);
  Alcotest.(check (option int)) "find 1" (Some 10) (H.find_opt 1 m);
  Alcotest.(check (option int)) "find 2" (Some 20) (H.find_opt 2 m);
  Alcotest.(check (option int)) "find 3" (Some 30) (H.find_opt 3 m)

let test_add_replace () =
  let m = H.empty ~hash:int_hash |> H.add 1 10 |> H.add 1 20 in
  Alcotest.(check int) "size after replace" 1 (H.size m);
  Alcotest.(check (option int)) "find replaced" (Some 20) (H.find_opt 1 m)

let test_remove () =
  let m = H.empty ~hash:int_hash |> H.add 1 10 |> H.add 2 20 |> H.remove 1 in
  Alcotest.(check int) "size after remove" 1 (H.size m);
  Alcotest.(check (option int)) "find removed" None (H.find_opt 1 m);
  Alcotest.(check (option int)) "find remaining" (Some 20) (H.find_opt 2 m)

let test_remove_nonexistent () =
  let m = H.empty ~hash:int_hash |> H.add 1 10 |> H.remove 2 in
  Alcotest.(check int) "size unchanged" 1 (H.size m);
  Alcotest.(check (option int)) "find unchanged" (Some 10) (H.find_opt 1 m)

let test_filter () =
  let m =
    H.empty ~hash:int_hash |> H.add 1 10 |> H.add 2 20 |> H.add 3 30
    |> H.filter (fun _ v -> v > 15)
  in
  Alcotest.(check int) "filtered size" 2 (H.size m);
  Alcotest.(check (option int)) "filtered out" None (H.find_opt 1 m);
  Alcotest.(check (option int)) "kept 1" (Some 20) (H.find_opt 2 m);
  Alcotest.(check (option int)) "kept 2" (Some 30) (H.find_opt 3 m)

let test_map () =
  let m =
    H.empty ~hash:int_hash |> H.add 1 10 |> H.add 2 20 |> H.map (( * ) 2)
  in
  Alcotest.(check int) "mapped size" 2 (H.size m);
  Alcotest.(check (option int)) "mapped value 1" (Some 20) (H.find_opt 1 m);
  Alcotest.(check (option int)) "mapped value 2" (Some 40) (H.find_opt 2 m)

let test_fold_left () =
  let m = H.empty ~hash:int_hash |> H.add 1 10 |> H.add 2 20 |> H.add 3 30 in
  let sum = H.fold_left (fun acc _ v -> acc + v) 0 m in
  Alcotest.(check int) "fold_left sum" 60 sum

let test_fold_right () =
  let m = H.empty ~hash:int_hash |> H.add 1 10 |> H.add 2 20 |> H.add 3 30 in
  let sum = H.fold_right (fun _ v acc -> acc + v) m 0 in
  Alcotest.(check int) "fold_right sum" 60 sum

let test_to_list () =
  let m = H.empty ~hash:int_hash |> H.add 1 10 |> H.add 2 20 in
  let lst = H.to_list m in
  Alcotest.(check int) "list length" 2 (List.length lst);
  Alcotest.(check bool) "contains (1, 10)" true (List.mem (1, 10) lst);
  Alcotest.(check bool) "contains (2, 20)" true (List.mem (2, 20) lst)

let test_of_list () =
  let m = H.of_list ~hash:int_hash [ (1, 10); (2, 20); (3, 30) ] in
  Alcotest.(check int) "of_list size" 3 (H.size m);
  Alcotest.(check (option int)) "of_list find 1" (Some 10) (H.find_opt 1 m);
  Alcotest.(check (option int)) "of_list find 2" (Some 20) (H.find_opt 2 m);
  Alcotest.(check (option int)) "of_list find 3" (Some 30) (H.find_opt 3 m)

let test_equal () =
  let m1 = H.empty ~hash:int_hash |> H.add 1 10 |> H.add 2 20 in
  let m2 = H.empty ~hash:int_hash |> H.add 2 20 |> H.add 1 10 in
  let m3 = H.empty ~hash:int_hash |> H.add 1 10 |> H.add 2 30 in
  Alcotest.(check bool) "equal maps" true (H.equal ( = ) m1 m2);
  Alcotest.(check bool) "different values" false (H.equal ( = ) m1 m3)

let test_concat () =
  let m1 = H.empty ~hash:int_hash |> H.add 1 10 |> H.add 2 20 in
  let m2 = H.empty ~hash:int_hash |> H.add 3 30 |> H.add 4 40 in
  let m3 = H.concat m1 m2 in
  Alcotest.(check int) "concat size" 4 (H.size m3);
  Alcotest.(check (option int)) "concat find 1" (Some 10) (H.find_opt 1 m3);
  Alcotest.(check (option int)) "concat find 4" (Some 40) (H.find_opt 4 m3)

let test_concat_overlap () =
  let m1 = H.empty ~hash:int_hash |> H.add 1 10 |> H.add 2 20 in
  let m2 = H.empty ~hash:int_hash |> H.add 2 99 |> H.add 3 30 in
  let m3 = H.concat m1 m2 in
  Alcotest.(check int) "concat overlap size" 3 (H.size m3);
  Alcotest.(check (option int)) "concat override" (Some 99) (H.find_opt 2 m3)

let test_monoid_identity () =
  let m = H.empty ~hash:int_hash |> H.add 1 10 |> H.add 2 20 in
  let e = H.empty ~hash:int_hash in
  let m_e = H.concat m e in
  let e_m = H.concat e m in
  Alcotest.(check bool) "right identity" true (H.equal ( = ) m m_e);
  Alcotest.(check bool) "left identity" true (H.equal ( = ) m e_m)

let test_immutability () =
  let m1 = H.empty ~hash:int_hash |> H.add 1 10 in
  let m2 = H.add 2 20 m1 in
  Alcotest.(check int) "original unchanged size" 1 (H.size m1);
  Alcotest.(check int) "new map size" 2 (H.size m2);
  Alcotest.(check (option int))
    "original unchanged value" (Some 10) (H.find_opt 1 m1);
  Alcotest.(check (option int)) "original has no new key" None (H.find_opt 2 m1)

let test_many_elements () =
  let rec add_range m start stop =
    if start > stop then m
    else add_range (H.add start (start * 10) m) (start + 1) stop
  in
  let m = add_range (H.empty ~hash:int_hash) 1 100 in
  Alcotest.(check int) "many elements size" 100 (H.size m);
  Alcotest.(check (option int)) "find in large map" (Some 500) (H.find_opt 50 m)

(* ========== Test Suite ========== *)

let unit_tests =
  [
    ("empty", `Quick, test_empty);
    ("add single", `Quick, test_add_single);
    ("add multiple", `Quick, test_add_multiple);
    ("add replace", `Quick, test_add_replace);
    ("remove", `Quick, test_remove);
    ("remove nonexistent", `Quick, test_remove_nonexistent);
    ("filter", `Quick, test_filter);
    ("map", `Quick, test_map);
    ("fold_left", `Quick, test_fold_left);
    ("fold_right", `Quick, test_fold_right);
    ("to_list", `Quick, test_to_list);
    ("of_list", `Quick, test_of_list);
    ("equal", `Quick, test_equal);
    ("concat", `Quick, test_concat);
    ("concat overlap", `Quick, test_concat_overlap);
    ("monoid identity", `Quick, test_monoid_identity);
    ("immutability", `Quick, test_immutability);
    ("many elements", `Quick, test_many_elements);
    ("string keys", `Quick, test_string_keys);
  ]

(** Main test runner *)
let () = Alcotest.run "HashMap Tests" [ ("Unit Tests", unit_tests) ]
