(** Probability distribution monad *)
open Num

type 'a m

(**
 * Create a new distribution from an (item, probability) assoc list.
 *
 * Probabilities are non-negative rationals ([Num.num]) and must sum to 1.
*)
val dist_of_al : ('a * num) list -> 'a m

(**
 * Unwrap a distribution into an (item, probability) assoc list.
*)
val al_of_dist : 'a m -> ('a * num) list

(**
 * Unwrap a distribution into an (item, float) assoc list, with the rational
 * probabilities being converted into their nearest floating point numbers.
 *
 * This method is more for display and testing purposes - you should NOT use
 * the return value for further processing. Specifically, due to floating
 * point semantics, the sum of these may not be exactly 1.
*)
val fal_of_dist : 'a m -> ('a * float) list

(**
 * Return a string representation of this distribution.
*)
val string_of_dist : ('a -> Sexplib.Sexp.t) -> 'a m -> string

(**
 * Condition a distribution on the given filter predicate.
 *
 * Returns [Some d, p] where [p] is the proportion of [m]'s event space that
 * matched, and [d] is the resulting (normalised) distribution; or [None, 0] if
 * the filter predicate matched no events.
*)
val given : ('a -> bool) -> 'a m -> 'a m option * num

(**
 * Calculate an expected value for this distribution, generically.
 *
 * The first arg is a "sum"-like binary operator. The second arg combines
 * each item and its probability, into an element to be summed. The third
 * argument is the distribution to apply these operations to.
*)
val expect_a : ('a -> 'a -> 'a) -> ('b -> num -> 'a) -> 'b m -> 'a

(**
 * Calculate an expected value for this distribution.
 *
 * The first argument is a function that converts each item into a rational
 * number which is the "value" for that item.
*)
val expect : ('a -> num) -> 'a m -> num

(**
 * Return the sole item or [None] if there are more possibilities.
*)
val certain : 'a m -> 'a option

(* include here to match implementation; in general we are always more likely to
 * implement more abstract functions in terms of specific functions *)
include BatInterfaces.Monad
  with type 'a m := 'a m
(*
 * Ideally we would constrain 'a to be of OrderedType. i.e. define OrdMonad
 * but this is not yet possible in OCaml; see
 *
 * https://arxiv.org/abs/1512.01895 (or https://www.lpw25.net/ml2014.pdf)
 * https://github.com/ocamllabs/ocaml-modular-implicits
 *
 * Once this is done, we can sort the internal distribution table, then
 * define a way to compare two distributions for equality.
 *
 * If we need to, we can make OrdMonad implement Monad by doing this:
 *
 * http://hsenag.livejournal.com/11803.html?nojs=1
*)

(** Alias for [Monad.bind]. *)
val (>>=) : 'a m -> ('a -> 'b m) -> 'b m
val join : 'a m m -> 'a m

val apply : ('a -> 'b) m -> 'a m -> 'b m
(** Alias for [Applicative.join]. *)
val (<*>) : ('a -> 'b) m -> 'a m -> 'b m

val map : ('a -> 'b) -> 'a m -> 'b m
(** Alias for [Functor.map]. *)
val (<$>) : ('a -> 'b) -> 'a m -> 'b m
(** Alias for flipped [Functor.map]. *)
val (<&>) : 'a m -> ('a -> 'b) -> 'b m

(* other common utils *)

val filter : ('a -> bool) -> 'a m -> 'a m
val exists : ('a -> bool) -> 'a m -> bool
val for_all : ('a -> bool) -> 'a m -> bool
