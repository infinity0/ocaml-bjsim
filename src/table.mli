(** State of a blackjack table *)

open Hand

(**
 * Data type of a table description.
 *
 * The type parameter ['s] is the data type of the shoe on the table.
*)
type 's desc

(**
 * Data type of a table.
 *
 * The type parameter ['s] is the data type of the shoe on the table.
*)
type 's t

(**
 * Module functions, as a record.
 *
 * The type parameter ['s] is the data type of the shoe on the table.
*)
type 's r = {
  new_table : int -> 's -> 's t;
  (**
   * Create a new table.
   *
   * The first argument is the number of decks to start with. The second
   * argument is the data type of the shoe; see {!Shoe.r}.
  *)

  num_players : 's t -> int;
  (**
   * Number of players, excluding the house, on the table
  *)

  all_players : 's t -> int list;
  (**
   * List of player indexes, in the order in which they play the game.
   *
   * The house player (index 0) is the last element.
  *)

  current_player : 's t -> int;
  (**
   * Index of the current player.
  *)

  hand_of_house : 's t -> hand;

  hand_of_player : int -> 's t -> hand;

  is_turn_finished : 's t -> bool;

  is_deal_complete : 's t -> bool;
  (**
   * Has everyone been dealt two cards?
  *)

  finish_turn : 's t -> 's t;
  (**
   * Finish the current player's turn.
   *
   * The caller should have already checked that [not @@ is_turn_finished t]
  *)

  next_turn : 's t -> 's t;
  (**
   * Move to the next player's turn.
   *
   * The caller should have already checked that [not @@ is_turn_finished t]
  *)

  next_turn_checked : 's t -> 's t;
  (**
   * Move to the next player's turn.
   *
   * Raises [Invalid_argument] if the current player's turn is not finished.
  *)

  next_game : 's t -> 's t;
  (**
   * Start the next game.
   *
   * Reset all players' hands, but keep the state of the shoe.
  *)

  hit : card option -> 's t -> 's t Prob.m;
  (**
   * Hit the current player, or do nothing if their hand cannot be hit.
   *
   * If the argument is [None], hit a random card based on the state of the
   * shoe. Otherwise, hit the card specified inside the [Some] container.
  *)

  hit_or_finish : (hand -> bool) -> card option -> 's t -> 's t Prob.m;
  (**
   * Hit the current player, or finish the turn if their hand cannot be hit.
   *
   * The first argument is an additional predicate that the hand must satisfy
   * in order for the hit to occur; if not then we finish their turn instead.
  *)

  describe_hands : 's t -> 's desc;
}

val make : 's Shoe.r -> 's r
