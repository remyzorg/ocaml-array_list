type 'a t

exception Out_of_bounds of string

val clear: 'a t -> unit

val append: 'a t -> 'a t -> bool


val of_array: 'a array -> 'a t
val make: int -> 'a -> 'a t
val copy: 'a t -> 'a t

val get: 'a t -> int -> 'a


val set: 'a t -> int -> 'a -> 'a


val add: 'a t -> int -> 'a -> unit


val remove: 'a t -> int -> 'a

val iter: ('a -> unit) -> 'a t -> unit

val iteri: (int -> 'a -> unit) -> 'a t -> unit

val fold_left: ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
val fold_right: ('b -> 'a -> 'a) -> 'b t -> 'a -> 'a

val to_list: 'a t -> 'a list
val of_list: 'a list -> 'a t
