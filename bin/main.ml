open Lab2
let string_hash s =
  let len = String.length s in
  let rec aux i acc =
    if i >= len then acc
    else
      let acc' = (acc * 31 + Char.code s.[i]) mod max_int in
      aux (i + 1) acc'
  in
  aux 0 0
let print_map_entry k v =
  Printf.printf "  %s -> %d\n" k v
let () =
  
  print_endline "1. Creating empty HashMap...";
  let empty_map = Hashmap.empty ~hash:string_hash in
  Printf.printf "   Size: %d\n\n" (Hashmap.size empty_map);
  
  print_endline "2. Adding elements (one, two, three)...";
  let map = empty_map
    |> Hashmap.add "one" 1
    |> Hashmap.add "two" 2
    |> Hashmap.add "three" 3 in
  Printf.printf "   Size: %d\n" (Hashmap.size map);
  print_endline "   Contents:";
  Hashmap.fold_left (fun () k v -> print_map_entry k v) () map;
  print_newline ();
  
  print_endline "3. Applying map (multiply by 10)...";
  let mapped = Hashmap.map (( * ) 10) map in
  print_endline "   Result:";
  Hashmap.fold_left (fun () k v -> print_map_entry k v) () mapped;
  print_newline ();
  
  print_endline "4. Filtering (value > 15)...";
  let filtered = Hashmap.filter (fun _ v -> v > 15) mapped in
  Printf.printf "   Size after filter: %d\n" (Hashmap.size filtered);
  print_endline "   Result:";
  Hashmap.fold_left (fun () k v -> print_map_entry k v) () filtered;
  print_newline ();
  
  print_endline "5. Testing monoid concat operation...";
  let map2 = Hashmap.empty ~hash:string_hash 
    |> Hashmap.add "four" 4 
    |> Hashmap.add "five" 5 in
  let concatenated = Hashmap.concat map map2 in
  Printf.printf "   Size after concat: %d\n" (Hashmap.size concatenated);
  print_endline "   Result:";
  Hashmap.fold_left (fun () k v -> print_map_entry k v) () concatenated;
  print_newline ();
  
  print_endline "6. Removing 'three'...";
  let after_remove = Hashmap.remove "three" map in
  Printf.printf "   Size after removal: %d\n" (Hashmap.size after_remove);
  print_endline "   Result:";
  Hashmap.fold_left (fun () k v -> print_map_entry k v) () after_remove;
  print_newline ();
  
  print_endline "7. Using fold_left to compute sum...";
  let sum = Hashmap.fold_left (fun acc _ v -> acc + v) 0 map in
  Printf.printf "   Sum of values: %d\n\n" sum;
  
  print_endline "8. Verifying immutability...";
  Printf.printf "   Original map size: %d (unchanged)\n" (Hashmap.size map);
  Printf.printf "   After operations, original still has 'three': %b\n\n" 
    (Hashmap.mem "three" map);