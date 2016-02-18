open OUnit2
open Hand
open Num
open Sim

let tc_basic ctx =
  let testSim = Sim.make2 Rule.bjs Shoe.realShoe in
  let _one = num_of_int 1 in
  let _zero = num_of_int 0 in
  let _neg_one = num_of_int (-1) in
  let {
    new_sim; deal_next_game; exec_round; payout_of_player; rule_r; table_r;
  } = testSim in
  let strat_stand_all p t =
    match p with
    | 0 -> rule_r.Rule.house_play t
    | _ -> Prob.return (table_r.Table.finish_turn t) in
  let payout_of cards =
    Lazy.force new_sim
    |> deal_next_game (List.map card_of_int cards)
    |> exec_round strat_stand_all
    |> payout_of_player rule_r.Rule.pay 1 in
  assert_equal (payout_of [10; 10; 10; 1]) _neg_one;
  assert_equal (payout_of [10; 10; 1; 1])  _zero;;


(** TODO: OUnit2 should detect test cases automatically. *)
let suite = "ts_sim" >::: [
    "tc_basic" >:: tc_basic;
  ]
