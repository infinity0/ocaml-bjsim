open OUnit2
open Hand
open Shoe

let suite = [
  "tc_basic" >:: (fun ctx ->
      let open RealShoe in
      let all_dealt s = List.map (num_dealt s) all_cards in
      assert_equal (all_dealt (new_shoe 6))
        [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
      let draw_one = List.map fst (Prob.al_of_dist (draw (new_shoe 6))) in
      assert_equal (all_dealt (List.assoc (card_of_int 1) draw_one))
        [1; 0; 0; 0; 0; 0; 0; 0; 0; 0];
      assert_equal (all_dealt (List.assoc (card_of_int 2) draw_one))
        [0; 1; 0; 0; 0; 0; 0; 0; 0; 0];
    );
]
