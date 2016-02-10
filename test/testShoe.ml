open OUnit2
open Hand
open Shoe

let tc_total ctx =
  let open RealShoe in
  let num_dealt d = List.map snd (cards_dealt d) in
  assert_equal (num_dealt (new_shoe 6))
    [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
  let draw_one = List.map fst (Prob.al_of_dist (draw (new_shoe 6))) in
  assert_equal (num_dealt (List.assoc (card_of_int 1) draw_one))
    [1; 0; 0; 0; 0; 0; 0; 0; 0; 0];
  assert_equal (num_dealt (List.assoc (card_of_int 2) draw_one))
    [0; 1; 0; 0; 0; 0; 0; 0; 0; 0];;


(** TODO: OUnit2 should detect test cases automatically. *)
let suite = "ts_shoe" >::: [
    "tc_basic" >:: tc_total;
  ]
