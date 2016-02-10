open OUnit2
open Prob
open Num

let assert_raises_any f = try
	f(); raise (Failure "succeeded unexpectedly")
  with | _ -> ()

let (/-) a b = num_of_int(a) // num_of_int(b)


let tc_basic_dedup ctx =
  let f i = dist_of_al [(i, 1 /- 2); (i * 2, 1 /- 2)] in
  let d0 = fal_of_dist (return 1 >>= f >>= f >>= f) in
  assert_equal (List.assoc 1 d0) 0.125;
  assert_equal (List.assoc 2 d0) 0.375;
  assert_equal (List.assoc 4 d0) 0.375;
  assert_equal (List.assoc 8 d0) 0.125;
  assert_equal (List.length d0) 4;;

let tc_raise_invalid ctx =
  assert_raises_any (fun () -> dist_of_al [(0, -1 /- 2); (1, 3 /- 2)]);
  assert_raises_any (fun () -> dist_of_al [(0, 1 /- 2); (1, 1 /- 4)]);;


(** TODO: OUnit2 should detect test cases automatically. *)
let suite = "ts_prob" >::: [
	  "tc_basic_dedup" >:: tc_basic_dedup;
	  "tc_raise_invalid" >:: tc_raise_invalid
]
