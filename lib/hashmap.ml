type ('k, 'v) entry = Empty | Tombstone | Entry of 'k * 'v

type ('k, 'v) t = {
  hash : 'k -> int;
  size : int;
  buckets : ('k, 'v) entry array;
}

let default_capacity = 16
let max_load_factor = 0.75
let capacity t = Array.length t.buckets
let load_factor t = float_of_int t.size /. float_of_int (capacity t)

let rec find_index_internal buckets hash_fn key index cap count =
  if count >= cap then None
  else
    match buckets.(index) with
    | Empty -> Some index
    | Tombstone ->
        let next_idx = (index + 1) mod cap in
        find_index_internal buckets hash_fn key next_idx cap (count + 1)
    | Entry (k, _) when k = key -> Some index
    | Entry _ ->
        let next_idx = (index + 1) mod cap in
        find_index_internal buckets hash_fn key next_idx cap (count + 1)

let find_index_for_insert buckets hash_fn key cap =
  let start_index = (hash_fn key |> abs) mod cap in
  match find_index_internal buckets hash_fn key start_index cap 0 with
  | Some idx -> idx
  | None -> start_index

let rec find_key_index buckets hash_fn key index cap count =
  if count >= cap then None
  else
    match buckets.(index) with
    | Empty -> None
    | Tombstone ->
        let next_idx = (index + 1) mod cap in
        find_key_index buckets hash_fn key next_idx cap (count + 1)
    | Entry (k, _) when k = key -> Some index
    | Entry _ ->
        let next_idx = (index + 1) mod cap in
        find_key_index buckets hash_fn key next_idx cap (count + 1)

let resize t new_cap =
  let entries =
    Array.fold_left
      (fun acc -> function Entry (k, v) -> (k, v) :: acc | _ -> acc)
      [] t.buckets
  in
  let empty_buckets = Array.make new_cap Empty in
  let final_buckets, final_size =
    List.fold_left
      (fun (buckets, n) (k, v) ->
        let idx = find_index_for_insert buckets t.hash k new_cap in
        let buckets' = Array.copy buckets in
        buckets'.(idx) <- Entry (k, v);
        (buckets', n + 1))
      (empty_buckets, 0) entries
  in
  { t with size = final_size; buckets = final_buckets }

let empty ~hash =
  { hash; size = 0; buckets = Array.make default_capacity Empty }

let size t = t.size

let find_opt key t =
  let cap = capacity t in
  let start_idx = (t.hash key |> abs) mod cap in
  match find_key_index t.buckets t.hash key start_idx cap 0 with
  | Some idx -> (
      match t.buckets.(idx) with Entry (_, v) -> Some v | _ -> None)
  | None -> None

let mem key t = match find_opt key t with Some _ -> true | None -> false

let add key value t =
  let t' =
    if load_factor t > max_load_factor then resize t (capacity t * 2) else t
  in
  let cap = capacity t' in
  let idx = find_index_for_insert t'.buckets t'.hash key cap in
  let existing = t'.buckets.(idx) in
  let new_size =
    match existing with
    | Entry (k, _) when k = key -> t'.size
    | _ -> t'.size + 1
  in
  let new_buckets =
    Array.init cap (fun i ->
        if i = idx then Entry (key, value) else t'.buckets.(i))
  in
  { t' with size = new_size; buckets = new_buckets }

let remove key t =
  let cap = capacity t in
  let start_idx = (t.hash key |> abs) mod cap in
  match find_key_index t.buckets t.hash key start_idx cap 0 with
  | Some idx ->
      let new_buckets = Array.copy t.buckets in
      new_buckets.(idx) <- Tombstone;
      { t with size = t.size - 1; buckets = new_buckets }
  | None -> t

let fold_left f acc t =
  Array.fold_left
    (fun current_acc entry ->
      match entry with Entry (k, v) -> f current_acc k v | _ -> current_acc)
    acc t.buckets

let to_list t = fold_left (fun acc k v -> (k, v) :: acc) [] t

let fold_right f t acc =
  let items = to_list t in
  List.fold_right (fun (k, v) acc' -> f k v acc') items acc

let filter pred t =
  fold_left
    (fun acc k v -> if pred k v then add k v acc else acc)
    (empty ~hash:t.hash) t

let map f t = fold_left (fun acc k v -> add k (f v) acc) (empty ~hash:t.hash) t
let concat t1 t2 = fold_left (fun acc k v -> add k v acc) t1 t2

let equal value_eq t1 t2 =
  if t1.size <> t2.size then false
  else
    fold_left
      (fun is_eq k1 v1 ->
        is_eq
        && match find_opt k1 t2 with Some v2 -> value_eq v1 v2 | None -> false)
      true t1

let of_list ~hash lst =
  List.fold_left (fun acc (k, v) -> add k v acc) (empty ~hash) lst
