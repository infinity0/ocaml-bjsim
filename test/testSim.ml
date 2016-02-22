open OUnit2
open Hand
open Num
open Sim

module TestSim = Make2 (Rule.BJS) (Shoe.RealShoe)

let tc_basic ctx =
  let _one = num_of_int 1 in
  let _zero = num_of_int 0 in
  let _neg_one = num_of_int (-1) in
  let strat_stand_all p t =
    match p with
    | 0 -> TestSim.Rule.house_play t
    | _ -> Prob.return (TestSim.Table.finish_turn t) in
  let open TestSim in
  let payout_of cards =
    new_sim 1 new_shoe_default
    |> deal_next_game (List.map card_of_int cards)
    |> exec_round strat_stand_all
    |> payout_of_player TestSim.Rule.pay 1 in
  assert_equal (payout_of [10; 10; 10; 1]) _neg_one;
  assert_equal (payout_of [10; 10; 1; 1])  _zero;;


(** TODO: OUnit2 should detect test cases automatically. *)
let suite = "ts_sim" >::: [
    "tc_basic" >:: tc_basic;
  ]
