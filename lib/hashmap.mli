type ('k, 'v) t

val empty : hash:('k -> int) -> ('k, 'v) t

val add : 'k -> 'v -> ('k, 'v) t -> ('k, 'v) t

val remove : 'k -> ('k, 'v) t -> ('k, 'v) t

val find_opt : 'k -> ('k, 'v) t -> 'v option

val mem : 'k -> ('k, 'v) t -> bool

val size : ('k, 'v) t -> int


val filter : ('k -> 'v -> bool) -> ('k, 'v) t -> ('k, 'v) t

val map : ('v -> 'w) -> ('k, 'v) t -> ('k, 'w) t

val fold_left : ('acc -> 'k -> 'v -> 'acc) -> 'acc -> ('k, 'v) t -> 'acc

val fold_right : ('k -> 'v -> 'acc -> 'acc) -> ('k, 'v) t -> 'acc -> 'acc

val concat : ('k, 'v) t -> ('k, 'v) t -> ('k, 'v) t

val equal : ('v -> 'v -> bool) -> ('k, 'v) t -> ('k, 'v) t -> bool

val to_list : ('k, 'v) t -> ('k * 'v) list

val of_list : hash:('k -> int) -> ('k * 'v) list -> ('k, 'v) t
