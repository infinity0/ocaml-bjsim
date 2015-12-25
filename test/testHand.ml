open OUnit2
open Hand

let tc_basic ctx =
  let n = cards_dealt in
  let z = new_hand in
  let c = card_of_int in
  assert_equal (n z) 0;
  assert_equal (n (z ++ (c 2))) 1;
  assert_equal (n (z ++ (c 1))) 1;
  assert_equal (n (z ++ (c 10))) 1;
  assert_equal (n (z ++ (c 1) ++ (c 10))) 2;
  assert_equal (n (z ++ (c 10) ++ (c 1))) 2;
  assert_equal (n (z ++ (c 10) ++ (c 1) ++ (c 2))) 3;
  assert_equal (int_of_hand (z ++ (c 1) ++ (c 10))) 21;
  assert_bool "blackjack" (is_nat (z ++ (c 1) ++ (c 10)));
  assert_bool "basic '=' test" ((c 1) = (c 1));;


(** TODO: OUnit2 should detect test cases automatically. *)
let suite = "ts_hand" >::: [
	  "tc_basic" >:: tc_basic;
]
