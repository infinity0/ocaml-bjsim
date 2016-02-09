open OUnit2
open Hand
open Num
open Rule

let tc_basic ctx =
  let h l = hand_of_card_list (List.map card_of_int l) in
  let _one = num_of_int 1 in
  let _zero = num_of_int 0 in
  let _neg_one = num_of_int (-1) in
  let _house_push_22 = fun h -> if is_22 h then _zero else _one in
  let bjs_pay h p = pay_base ~nat_payout:_one ~house_bust:_house_push_22 h p in
  assert_equal (is_nat (h [7])) false;
  assert_equal (bjs_pay (h [10; 1]) (h [1; 10])) _zero;
  assert_equal (bjs_pay (h [10; 1]) (h [10; 10])) _neg_one;
  assert_equal (bjs_pay (h [7; 7; 8]) (h [1; 10])) _one;
  assert_equal (bjs_pay (h [7; 7; 8]) (h [4; 7; 10])) _zero;
  assert_equal (bjs_pay (h [7; 7; 8]) (h [7])) _zero;
  assert_equal (bjs_pay (h [7]) (h [7; 7; 8])) _neg_one;
  assert_equal (bjs_pay (h [7]) (h [7; 7; 6])) _one;
  assert_equal (bjs_pay (h [7; 7; 6]) (h [7])) _neg_one;;


(** TODO: OUnit2 should detect test cases automatically. *)
let suite = "ts_rule" >::: [
	  "tc_basic" >:: tc_basic;
]
