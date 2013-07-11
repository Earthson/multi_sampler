type t = {
  k : int;
  mutable f : int -> float;
  p : float array;
  sums : float array;
  stats : bool array;
  mdfy_stack : int Stack.t;
}

val init : t -> (int -> float) -> unit
val make : (int -> float) -> int -> t
val gen : t -> int
val to_stack : t -> int -> unit
val to_set : t -> int -> float -> unit
val with_stack : t -> unit
val set : t -> int -> float -> unit
val clear : t -> unit
