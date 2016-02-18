(** Different ways of tracking or counting the state of the cards in play *)

open Hand

(** State of the remaining cards in play. *)
module type S = sig
  type t

  (**
   * Create a new full shoe.
   *
   * The first argument is the number of decks (52 cards) in the shoe.
  *)
  val new_shoe : int -> t

  (**
   * Probability distribution of drawing each card from the shoe.
  *)
  val card_prob : t -> card Prob.m

  (**
   * Draw a specific card from the shoe, returning the next state.
   *
   * Obviously this is not realistic, and is only really meant for testing and
   * to help implement {!draw} and {!Sim.S.deal_next_game}.
   *
   * Raises [Invalid_argument] if the card is not available.
  *)
  val draw_card : card -> t -> t

  (**
   * Draw a card randomly from the shoe.
  *)
  val draw : t -> (card * t) Prob.m

  (**
   * Return a string representation of the deck.
  *)
  val string_of_shoe : t -> string

  val shoe_of_string : string -> t
end

(**
 * Automatically derive a {!Shoe.S.draw} implementation from {!Shoe.S.draw_card}
 * and {!Shoe.S.card_prob} implementations.
*)
val derive_draw :
  (card -> 'a -> 'a) ->
  ('a -> card Prob.m) ->
  ('a -> (card * 'a) Prob.m)

(**
 * A shoe that (depending on your perspective) loses track of the cards that it
 * has already dealt, or has infinite decks, or a view of a non-counting player.
 *
 * The only valid input to {!Shoe.S.shoe_of_string} is [()].
*)
module VoidShoe : S

(**
 * The actual state of the cards, or (alternatively) a perfect counter's view.
 *
 * Example of a valid input to {!Shoe.S.shoe_of_string} is
 *
 * [((num_decks n) (num_dealt (a x x x x x x x x j)))]
 *
 * where [a] and [j] are the number of aces and 10/j/q/k cards respectively that
 * have already been dealt (i.e. are no longer in the shoe), and the [x] are the
 * number of [2..9] cards that have been dealt, in that order.
*)
module RealShoe : sig
  include S

  (**
   * The number of times a given card has been dealt so far, for a given shoe.
  *)
  val num_dealt : t -> card -> int
end


val of_name : string -> (module S) option
val to_name : (module S) -> string option
val all_names : string list
