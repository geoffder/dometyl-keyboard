(** compose *)
val ( >> ) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c

val ( let* ) : 'a option -> ('a -> 'b option) -> 'b option
val ( and* ) : 'a option -> 'b option -> ('a * 'b) option
val ( let+ ) : 'a option -> ('a -> 'b) -> 'b option
val ( and+ ) : 'a option -> 'b option -> ('a * 'b) option
