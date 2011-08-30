(** Extended List Module. *)

val iteri : (int -> 'a -> unit) -> 'a list -> unit
val sub : int -> int -> 'a list -> 'a list
val take : int -> 'a list -> 'a list
val drop : int -> 'a list -> 'a list
val drop_while : ('a -> bool) -> 'a list -> 'a list
val delete_nth : int -> 'a list -> 'a list
val delete_nth_naive : int -> 'a list -> 'a list
val is_empty : 'a list -> bool
val single : 'a list -> 'a
val last : 'a list -> 'a
val to_chain : 'a list -> ('a * 'a) list
val filter_some : 'a option list -> 'a list
val print_int_list : int list -> unit
val print_float_list : int list -> unit

(** remove cond xs removes the first element satisfies cond from xs.*)
val remove : ('a -> bool) -> 'a list -> 'a list
val find_max : ?comp:('b -> 'b -> int) -> ('a -> 'b) -> 'a list -> 'a
val find_min : ?comp:('b -> 'b -> int) -> ('a -> 'b) -> 'a list -> 'a
val find_max_val : ?comp:('b -> 'b -> int) -> ('a -> 'b) -> 'a list -> 'b
val find_min_val : ?comp:('b -> 'b -> int) -> ('a -> 'b) -> 'a list -> 'b
val mapi : (int -> 'a -> 'b) -> 'a list -> 'b list
val count : ('a -> bool) -> 'a list -> int
val rev_flatten : 'a list list -> 'a list
val flatten : 'a list list -> 'a list
val concat : 'a list list -> 'a list
val init : (int -> 'a) -> int -> 'a list
val make : int -> 'a -> 'a list
val map_orig : ('a -> 'b) -> 'a list -> 'b list
val map : ('a -> 'b) -> 'a list -> 'b list
val average_first : int -> float list -> float
val average : float list -> float
val split_while : ('a -> bool) -> 'a list -> 'a list * 'a list
val range : int -> int -> int -> int list
val take_ns : int list -> 'a list -> 'a list
val random_permutation : int -> int list
val remove_adjacent : 'a list -> 'a list
val remove_adjacent_tuple : int -> 'a list -> 'a list
val n_divide : int -> int -> int list list

(** ex. combination 4 2 returns [1001; 0101; 0011; 1010; 0110; 1100] (in binary). *)
val combination : int -> int -> int list

(** If x appeared earlier than y in the list and f x = f y then y is removed. The order of the list is not preserved. *)
val remove_duplicated : ('a -> 'b) -> 'a list -> 'a list
