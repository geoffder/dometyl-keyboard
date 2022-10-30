(** [g >> f]

    Compose the functions [f] and [g]. Equivalent to [fun x -> f (g x)]. *)
val ( >> ) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c

(** [Option.t] [let] binding operators *)

val ( let* ) : 'a option -> ('a -> 'b option) -> 'b option
val ( and* ) : 'a option -> 'b option -> ('a * 'b) option
val ( let+ ) : 'a option -> ('a -> 'b) -> 'b option
val ( and+ ) : 'a option -> 'b option -> ('a * 'b) option
