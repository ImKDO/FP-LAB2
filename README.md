# Лабораторная работа №2: Immutable HashMap

**Выполнил:** Кравченко Дмитрий Олегович

**Группа:** P3319

**Вариант:** oa-dict

## Описание

Реализация неизменяемой полиморфной структуры данных HashMap на языке OCaml с использованием открытой адресации для разрешения коллизий.

### Реализованные операции

#### Базовые операции

- `empty` - создание пустой HashMap
- `add` - добавление элемента (замена при существующем ключе)
- `remove` - удаление элемента по ключу
- `find_opt` - поиск значения по ключу
- `mem` - проверка наличия ключа
- `size` - получение количества элементов

#### Функции высшего порядка

- `filter` - фильтрация элементов по предикату
- `map` - отображение (преобразование значений)
- `fold_left` - левая свёртка
- `fold_right` - правая свёртка

#### Операции моноида

- `concat` - слияние двух HashMap (ассоциативная операция)
- `empty` - нейтральный элемент моноида

#### Дополнительные операции

- `equal` - сравнение двух HashMap

### Ключевые элементы реализации (минимальные комментарии)

```ocaml
(* Основные типы *)
type ('k,'v) slot =
  | Empty
  | Deleted
  | Occupied of 'k * 'v * int

type ('k,'v) t = {
  table : ('k,'v) slot array;
  size  : int;
  cap   : int;
}

let initial_capacity = 16
let load_factor = 0.75
```

```ocaml
(* Вспомогательные функции *)
let hash h k = (h k) land max_int
let next_pow_two n = (* округление вверх до степени двух *)
  let rec aux p = if p >= n then p else aux (p * 2) in aux 1
```

```ocaml
(* Создание пустой таблицы *)
let empty ?(h = Hashtbl.hash) () =
  { table = Array.make initial_capacity Empty; size = 0; cap = initial_capacity }
```

```ocaml
(* add возвращает новый HashMap *)
let add key value t =
  let t' =
    if load_factor t > max_load_factor then
      resize t (capacity t * 2)
    else t
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
      if i = idx then Entry (key, value)
      else t'.buckets.(i)
    )
  in
  { t' with size = new_size; buckets = new_buckets }
```

```ocaml
(* remove *)
let remove key t =
  let cap = capacity t in
  let start_idx = (t.hash key |> abs) mod cap in
  match find_key_index t.buckets t.hash key start_idx cap 0 with
  | Some idx ->
      let new_buckets = Array.copy t.buckets in
      new_buckets.(idx) <- Tombstone;
      { t with size = t.size - 1; buckets = new_buckets }
  | None -> t
```

```ocaml
(* find_opt, mem, size *)
let find_opt key t =
  let cap = capacity t in
  let start_idx = (t.hash key |> abs) mod cap in
  match find_key_index t.buckets t.hash key start_idx cap 0 with
  | Some idx -> (match t.buckets.(idx) with
                 | Entry (_, v) -> Some v
                 | _ -> None)
  | None -> None
```

```ocaml
(* concat и equal *)
let concat t1 t2 =
  fold_left (fun acc k v -> add k v acc) t1 t2

let equal value_eq t1 t2 =
  if t1.size <> t2.size then false
  else
    fold_left (fun is_eq k1 v1 ->
      is_eq && match find_opt k1 t2 with
      | Some v2 -> value_eq v1 v2
      | None -> false
    ) true t1

```

Краткие комментарии — только пояснения к ключевым решениям: open addressing, tombstones (Deleted), копирование массива при изменении (иммутабельность), рехеширование при превышении load factor.

## Свойства моноида

HashMap образует моноид относительно операции `concat`:

1. **Ассоциативность**: `(m1 ⊕ m2) ⊕ m3 = m1 ⊕ (m2 ⊕ m3)`
2. **Левый нейтральный элемент**: `∅ ⊕ m = m`
3. **Правый нейтральный элемент**: `m ⊕ ∅ = m`

где `⊕` = `concat`, `∅` = `empty`

### Unit-тесты (18 тестов)

Покрывают базовую функциональность:

- Создание пустой HashMap
- Добавление и удаление элементов
- Замена значений
- Фильтрация и отображение
- Свёртки (fold_left, fold_right)
- Конверсия в/из списков
- Сравнение HashMap
- Слияние (concat)
- Свойства моноида
- Неизменяемость структуры
- Работу с большим количеством элементов

### Property-based тесты (10 свойств)

Используется библиотека QCheck для генерации случайных данных:

1. **prop_add_find**: добавление и последующий поиск возвращает значение
2. **prop_add_size**: размер соответствует количеству уникальных ключей
3. **prop_remove_find**: после удаления элемент не найден
4. **prop_to_from_list**: конверсия в список и обратно сохраняет структуру
5. **prop_monoid_left_identity**: левый нейтральный элемент
6. **prop_monoid_right_identity**: правый нейтральный элемент
7. **prop_monoid_associativity**: ассоциативность concat
8. **prop_map_size**: map сохраняет размер
9. **prop_filter_size**: filter уменьшает или сохраняет размер
10. **prop_fold_sum**: fold_left корректно вычисляет сумму

## Сложность операций

| Операция | Средний случай | Худший случай |
| -------- | -------------- | ------------- |
| add      | O(1)           | O(n)          |
| remove   | O(1)           | O(n)          |
| find_opt | O(1)           | O(n)          |
| filter   | O(n)           | O(n)          |
| map      | O(n)           | O(n)          |
| fold     | O(n)           | O(n)          |
| concat   | O(n+m)         | O(n+m)        |
| equal    | O(n)           | O(n)          |

где n - размер HashMap, m - размер второй HashMap (для concat)
