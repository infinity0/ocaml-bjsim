open OUnit2
open Hand

let tc_basic ctx =
  let n = cards_dealt in
  let h l = hand_of_card_list (List.map card_of_int l) in
  assert_equal (n (h [])) 0;
  assert_equal (n (h [2])) 1;
  assert_equal (n (h [1])) 1;
  assert_equal (n (h [10])) 1;
  assert_equal (n (h [1; 10])) 2;
  assert_equal (n (h [10; 1])) 2;
  assert_equal (n (h [10; 1; 2])) 3;
  assert_equal (int_of_hand (h [1; 10])) 21;
  assert_bool "blackjack" (is_nat (h [1; 10]));
  assert_bool "basic '=' test" ((card_of_int 1) = (card_of_int 1));;


(** TODO: OUnit2 should detect test cases automatically. *)
let suite = "ts_hand" >::: [
    "tc_basic" >:: tc_basic;
  ]
