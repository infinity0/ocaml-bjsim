open OUnit2

(** TODO: OUnit2 should detect test suites automatically. *)
let all_suites = [
  "Hand" >::: TestHand.suite;
  "Prob" >::: TestProb.suite;
  "Shoe" >::: TestShoe.suite;
  "Rule" >::: TestRule.suite;
  "Sim" >::: TestSim.suite;
]

let () = run_test_tt_main ("all" >::: all_suites)
