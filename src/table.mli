(** State of a blackjack table *)

open Hand

(** Module signature *)
module type S = sig
	(** Data type of the shoe on the table *)
	type shoe_t

	(** Data type of the table *)
	type t

	(**
	 * Create a new table.
	 *
	 * The first argument is the number of decks to start with. The second
	 * argument is the data type of the shoe; see {!Shoe}.
	 *)
	val new_table : int -> shoe_t -> t

	(**
	 * Number of players, excluding the house, on the table
	 *)
	val num_players : t -> int

	(**
	 * List of player indexes, in the order in which they play the game.
	 *
	 * The house player (index 0) is the last element.
	 *)
	val all_players : t -> int list

	(**
	 * Index of the current player.
	 *)
	val current_player : t -> int

	val hand_of_house : t -> hand

	val hand_of_player : int -> t -> hand

	val is_turn_finished : t -> bool

	(**
	 * Has everyone been dealt two cards?
	 *)
	val is_deal_complete : t -> bool

	(**
	 * Finish the current player's turn.
	 *
	 * The caller should have already checked that [not @@ is_turn_finished t]
	 *)
	val finish_turn : t -> t

	(**
	 * Move to the next player's turn.
	 *
	 * The caller should have already checked that [not @@ is_turn_finished t]
	 *)
	val next_turn : t -> t

	(**
	 * Move to the next player's turn.
	 *
	 * Raises [Invalid_argument] if the current player's turn is not finished.
	 *)
	val next_turn_checked : t -> t

	(**
	 * Start the next game.
	 *
	 * Reset all players' hands, but keep the state of the shoe.
	 *)
	val next_game : t -> t

	(**
	 * Hit the current player, or do nothing if their hand cannot be hit.
	 *
	 * If the argument is [None], hit a random card based on the state of the
	 * shoe. Otherwise, hit the card specified inside the [Some] container.
	 *)
	val hit : card option -> t -> t Prob.m

	(**
	 * Hit the current player, or finish the turn if their hand cannot be hit.
	 *
	 * The first argument is an additional predicate that the hand must satisfy
	 * in order for the hit to occur; if not then we finish their turn instead.
	 *)
	val hit_or_finish : (hand -> bool) -> card option -> t -> t Prob.m
  end

(**
 * Data type of table as returned by {!Table.Make}.
 *
 * The first type parameter is the data type of the shoe on the table.
 *)
type 'shoe_t default_t

module Make :
functor (Shoe : Shoe.S) ->
S with type shoe_t = Shoe.t and
	   type t = Shoe.t default_t
