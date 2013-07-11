type t = {
  k : int;
  mutable f : int -> float;
  p : float array;
  sums : float array;
  stats : bool array;
  mdfy_stack : int Stack.t;
}

(*init: reinit a given Sampler with function (int->float)*)
val init : t -> (int -> float) -> unit

(*make: generate a sampler with function and its length
f: (int -> float): return the probility of given integer
n: range(0, n) for given f(other value is expected to be zero
*)
val make : (int -> float) -> int -> t

(*generate a integer with given sampler
O(log(n))
*)
val gen : t -> int

(*cache change to stack*)
val to_set : t -> int -> float -> unit

(*process changes cached in stack*)
val with_stack : t -> unit

(*process change without stack
please be sure that stack is processed
*)
val set : t -> int -> float -> unit

(*clear all changes*)
val clear : t -> unit
