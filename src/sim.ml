open Hand
open Prob
open Num

module type S = sig
	type shoe_t
	type t
	type m = t Prob.m
	module Rule : Rule.S with type table_t = t
	module Table : Table.S with type t = t
	val new_sim_with_shoe : int -> shoe_t -> m
	val new_sim_with_num_decks : int -> int -> m
	val new_sim : m Lazy.t
	val payout_of_player :(hand -> hand -> num) -> int -> m -> num
	val exec_turn : (int -> t -> m) -> m -> m
	val exec_round : (int -> t -> m) -> m -> m
	val deal_next_game : card list -> m -> m
  end

module Make
		 (Rule: Rule.S)
		 (Table: Table.S with type t = Rule.table_t)
		 (Shoe: Shoe.S with type t = Table.shoe_t) :
S with type shoe_t = Table.shoe_t and
	   type t = Table.t = struct
	open Table

	type shoe_t = Shoe.t
	type t = Table.t
	type m = t Prob.m

	module Rule = Rule
	module Table = Table

	let new_sim_with_shoe players shoe =
	  return (new_table players shoe)

	let new_sim_with_num_decks players num_decks =
	  new_sim_with_shoe players (Shoe.new_shoe num_decks)

	let new_sim = lazy (new_sim_with_shoe 1 (Shoe.new_shoe Rule.default_num_decks))

	let payout_of_player f player m =
	  expect (fun gs -> f (hand_of_house gs) (hand_of_player player gs)) m

	let rec _do_strat_until_finished f m =
	  if for_all is_turn_finished m then m
	  else m >>= f |> _do_strat_until_finished f

	let _exec_turn strat_of_player player m = (* assume `player` is correct *)
	  if exists is_turn_finished m then
		raise (Invalid_argument "a turn is already finished")
	  else
		m
		|> _do_strat_until_finished
			 (fun t ->
			  if is_turn_finished t then return t
			  else let m = (strat_of_player player) t in
				   if for_all (fun t -> current_player t == player) m then m
				   else raise (Invalid_argument "strat advanced to next player"))
		|> map next_turn (* already checked by _do_strat_until_finished *)

	let _get_player_for_exec m =
	  if not @@ for_all is_deal_complete m then
		raise (Invalid_argument "cards are not all dealt")
	  else
		match certain (map current_player m) with
		| None -> raise (Invalid_argument "not all on same player's turn")
		| Some player -> player

	let exec_turn strat_of_player m =
	  let player = _get_player_for_exec m in
	  m |> _exec_turn strat_of_player player

	let rec exec_round strat_of_player m =
	  let player = _get_player_for_exec m in
	  let next_m = m |> _exec_turn strat_of_player player in
	  if player = 0 then next_m (* stop recursing after house turn *)
	  else next_m |> exec_round strat_of_player

	let rec _do_deal_until_complete cards m =
	  (* during the deal we bypass the turn_finished flag, and don't check it *)
	  assert (not @@ exists is_turn_finished m);
	  if for_all is_deal_complete m then m
	  else let co, rest = match cards with
			 | [] -> None, []
			 | c :: rest -> Some(c), rest in
		   m >>= hit co |> map next_turn |> _do_deal_until_complete rest

	let deal_next_game cards m =
	  m |> map next_game |> _do_deal_until_complete cards

  end

module Make2
		 (MakeRule: Rule.MakeS)
		 (Shoe: Shoe.S) :
S with type shoe_t = Shoe.t and
	   type t = Shoe.t Table.default_t = struct
	module DTable = Table.Make(Shoe)
	include Make (MakeRule(DTable)) (DTable) (Shoe)
  end
