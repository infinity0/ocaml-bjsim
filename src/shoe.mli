(** Different ways of tracking or counting the state of the cards in play *)

open Hand

(**
 * Module functions, as a record.
 *
 * The type parameter ['s] is the data type of the shoe.
*)
type 's r = {
  new_shoe : int -> 's;
  (**
   * Create a new full shoe.
   *
   * The first argument is the number of decks (52 cards) in the shoe.
  *)

  card_prob : 's -> card Prob.m;
  (**
   * Probability distribution of drawing each card from the shoe.
  *)

  num_dealt : 's -> card -> int;
  (**
   * The number of times a given card has been dealt so far, for a given shoe.
  *)

  draw_card : card -> 's -> 's;
  (**
   * Draw a specific card from the shoe, returning the next state.
   *
   * Obviously this is not realistic, and is only really meant for testing and
   * to help implement {!Shoe.r.draw} and {!Sim.r.deal_next_game}.
   *
   * Raises [Invalid_argument] if the card is not available.
  *)

  draw : 's -> (card * 's) Prob.m;
  (**
   * Draw a card randomly from the shoe.
  *)

  string_of_shoe : 's -> string;
  (**
   * Return a string representation of the deck.
  *)

  shoe_of_string : string -> 's;
}

(**
 * Automatically derive a {!Shoe.r.draw} implementation from {!Shoe.r.draw_card}
 * and {!Shoe.r.card_prob} implementations.
*)
val derive_draw :
  (card -> 's -> 's) -> ('s -> card Prob.m) -> 's -> (card * 's) Prob.m

type voidShoe_t
(**
 * A shoe that (depending on your perspective) loses track of the cards that it
 * has already dealt, or has infinite decks, or a view of a non-counting player.
 *
 * The only valid input to {!Shoe.r.shoe_of_string} is [()].
*)
val voidShoe : voidShoe_t r

type realShoe_t
(**
 * The actual state of the cards, or (alternatively) a perfect counter's view.
 *
 * Example of a valid input to {!Shoe.r.shoe_of_string} is
 *
 * [((num_decks n) (num_dealt (a x x x x x x x x j)))]
 *
 * where [a] and [j] are the number of aces and 10/j/q/k cards respectively that
 * have already been dealt (i.e. are no longer in the shoe), and the [x] are the
 * number of [2..9] cards that have been dealt, in that order.
*)
val realShoe : realShoe_t r
