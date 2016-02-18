open Hand
open Prob
open Num

type 's tt = 's Table.t
type 's tm = 's Table.t Prob.m
type 's r = {
  rule_r : 's Rule.r;
  table_r : 's Table.r;
  new_sim_with_shoe : int -> 's -> 's tm;
  new_sim_with_num_decks : int -> int -> 's tm;
  new_sim : 's tm Lazy.t;
  payout_of_player :(hand -> hand -> num) -> int -> 's tm -> num;
  exec_turn : (int -> 's tt -> 's tm) -> 's tm -> 's tm;
  exec_round : (int -> 's tt -> 's tm) -> 's tm -> 's tm;
  deal_next_game : card list -> 's tm -> 's tm;
}

let make rule_r table_r shoe_r : 's r =

  let open Table in let {
    new_table; current_player;
    hand_of_house; hand_of_player;
    is_turn_finished; is_deal_complete;
    next_turn; next_game; hit;
  } = table_r in

  let new_sim_with_shoe players shoe =
    return (new_table players shoe) in

  let new_sim_with_num_decks players num_decks =
    new_sim_with_shoe players (shoe_r.Shoe.new_shoe num_decks) in

  let new_sim = lazy (new_sim_with_shoe 1 (shoe_r.Shoe.new_shoe rule_r.Rule.default_num_decks)) in

  let payout_of_player f player m =
    expect (fun gs -> f (hand_of_house gs) (hand_of_player player gs)) m in

  let rec _do_strat_until_finished f m =
    if for_all is_turn_finished m then m
    else m >>= f |> _do_strat_until_finished f in

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
      |> map next_turn (* already checked by _do_strat_until_finished *) in

  let _get_player_for_exec m =
    if not @@ for_all is_deal_complete m then
      raise (Invalid_argument "cards are not all dealt")
    else
      match certain (map current_player m) with
      | None -> raise (Invalid_argument "not all on same player's turn")
      | Some player -> player in

  let exec_turn strat_of_player m =
    let player = _get_player_for_exec m in
    m |> _exec_turn strat_of_player player in

  let rec exec_round strat_of_player m =
    let player = _get_player_for_exec m in
    let next_m = m |> _exec_turn strat_of_player player in
    if player = 0 then next_m (* stop recursing after house turn *)
    else next_m |> exec_round strat_of_player in

  let rec _do_deal_until_complete cards m =
    (* during the deal we bypass the turn_finished flag, and don's check it *)
    assert (not @@ exists is_turn_finished m);
    if for_all is_deal_complete m then
      m
    else
      let co, rest = match cards with
        | [] -> None, []
        | c :: rest -> Some(c), rest in
      m >>= hit co |> map next_turn |> _do_deal_until_complete rest in

  let deal_next_game cards m =
    m |> map next_game |> _do_deal_until_complete cards in

  {
    rule_r;
    table_r;
    new_sim_with_shoe;
    new_sim_with_num_decks;
    new_sim;
    payout_of_player;
    exec_turn;
    exec_round;
    deal_next_game;
  }

let make2 make_rule_r shoe_t : 's r =
  let table_r = Table.make shoe_t in
  make (make_rule_r table_r) table_r shoe_t
