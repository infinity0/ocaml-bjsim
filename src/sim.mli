(** A probabilistic simulation over a blackjack game *)

open Hand
open Prob
open Num

(** Module signature *)
module type S = sig
  (** Data type of the shoe on the table *)
  type shoe_t

  (** Data type of the table *)
  type t

  (** Probability distribution over possible table states *)
  type m = t Prob.m

  module Rule : Rule.S with type table_t = t

  module Table : Table.S with type t = t

  module Shoe : Shoe.S with type t = shoe_t

  (**
   * Create a new shoe with the default number of decks for the rule in play.
   *
   * For more ways to create a custom shoe, see the {!Shoe} submodule.
  *)
  val new_shoe_default : shoe_t

  (**
   * Start a new simulation on a new table.
   *
   * The first argument is the number of players, the second argument is the
   * shoe to start the simulation with.
  *)
  val new_sim : int -> shoe_t -> m

  (**
   * Calculate the expected payout by the house for a given player.
   *
   * The first argument is a 2-arg function (house hand, player hand) that
   * returns the payout for that specific player hand. The second argument is
   * the index of the subject player.
  *)
  val payout_of_player : (hand -> hand -> num) -> int -> m -> num

  val check_current_player : int -> m -> m

  (**
   * Execute a single player's turn. Execution ends after all possible games
   * (over the distribution of cards dealt) have finished that player's turn.
   *
   * The first argument is a strategy selector - given a player index, return
   * a function that takes the current table state, and returns a distribution
   * over the next table state.
  *)
  val exec_turn : (int -> t -> m) -> m -> m

  (**
   * Execute a round of the game, i.e. all player's turns. Execution ends
   * after the house player finishes their turn.
   *
   * The first argument is a strategy selector - given a player index, return
   * a function that takes the current table state, and returns a distribution
   * over the next table state.
  *)
  val exec_round : (int -> t -> m) -> m -> m

  (**
   * Start the next game and deal two cards to everyone.
   *
   * The first argument is a list of specific cards to deal to players, in
   * the same order as they would be dealt in a normal game. When the list is
   * exhausted, random cards are dealt from that point onwards.
  *)
  val deal_next_game : card list -> m -> m
end

module Make :
  functor (Rule : Rule.S) ->
  functor (Table : Table.S with type t = Rule.table_t) ->
  functor (Shoe : Shoe.S with type t = Table.shoe_t) ->
    S with type shoe_t = Table.shoe_t
       and type t = Table.t

module Make2 :
  functor (MakeRule : Rule.MakeS) ->
  functor (Shoe : Shoe.S) ->
    S with type shoe_t = Shoe.t
       and type t = Shoe.t Table.default_t
