(** A probabilistic simulation over a blackjack game *)

open Hand
open Prob
open Num

type 's tt = 's Table.t
type 's tm = 's Table.t Prob.m

(**
 * Module functions, as a record.
 *
 * The type parameter ['s] is the data type of the shoe on the table.
*)
type 's r = {
  rule_r : 's Rule.r;
  (** [Rule] module functions, as a record. *)

  table_r : 's Table.r;
  (** [Table] module functions, as a record. *)

  new_sim_with_shoe : int -> 's -> 's tm;
  (**
   * Start a new simulation on a new table, with a particular shoe.
   *
   * The first argument is the number of players.
  *)

  new_sim_with_num_decks : int -> int -> 's tm;
  (**
   * Start a new simulation on a new table, with a given number of decks.
   *
   * The first argument is the number of players.
  *)

  new_sim : 's tm Lazy.t;
  (**
   * Start a new simulation on a new table, with the house and one player,
   * and with the default number of decks for the rule in play.
  *)

  payout_of_player : (hand -> hand -> num) -> int -> 's tm -> num;
  (**
   * Calculate the expected payout by the house for a given player.
   *
   * The first argument is a 2-arg function (house hand, player hand) that
   * returns the payout for that specific player hand. The second argument is
   * the index of the subject player.
  *)

  exec_turn : (int -> 's tt -> 's tm) -> 's tm -> 's tm;
  (**
   * Execute a single player's turn. Execution ends after all possible games
   * (over the distribution of cards dealt) have finished that player's turn.
   *
   * The first argument is a strategy selector - given a player index, return
   * a function that takes the current table state, and returns a distribution
   * over the next table state.
  *)

  exec_round : (int -> 's tt -> 's tm) -> 's tm -> 's tm;
  (**
   * Execute a round of the game, i.e. all player's turns. Execution ends
   * after the house player finishes their turn.
   *
   * The first argument is a strategy selector - given a player index, return
   * a function that takes the current table state, and returns a distribution
   * over the next table state.
  *)

  deal_next_game : card list -> 's tm -> 's tm;
  (**
   * Start the next game and deal two cards to everyone.
   *
   * The first argument is a list of specific cards to deal to players, in
   * the same order as they would be dealt in a normal game. When the list is
   * exhausted, random cards are dealt from that point onwards.
  *)
}

val make : 's Rule.r -> 's Table.r -> 's Shoe.r -> 's r

val make2 : ('s Table.r -> 's Rule.r) -> 's Shoe.r -> 's r
