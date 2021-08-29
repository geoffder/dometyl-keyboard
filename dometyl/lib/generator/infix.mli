open! Base

(** compose *)
val ( >> ) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c
