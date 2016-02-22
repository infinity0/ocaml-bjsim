open Hand
open Num
open List
open Printf
open Sexplib.Std
open Sexplib_num.Std.Num
open Sim
open String

let map = List.map
let iter = List.iter

let c_open =
  let open Rule in [
    Hit, "\x1b[42m";
    Stand, "\x1b[41m";
    Surrender, "\x1b[45m";
    Double, "\x1b[46m\x1b[30m";
    Split, "\x1b[43m\x1b[30m";
  ]

let c_close = "\x1b[0m"

let (///) a b = num_of_int a // num_of_int b
let eps = 1 /// 10000

let ljust num char str = sub (concat "" [str; (make num char)]) 0 num
let rjust num char str =
  let s = concat "" [(make num char); str] in
  let l = length s in sub s (l-num) num

type payouts_hum = Pay of (char * string) list with sexp
let string_of_payouts_hum payouts =
  let payouts = map (fun (a, p) -> Rule.char_of_action a, approx_num_fix 8 p) payouts in
  Sexplib.Sexp.to_string (sexp_of_payouts_hum (Pay payouts))

type payouts_mach = Pay of (Rule.action * num) list with sexp
let string_of_payouts_mach payouts =
  Sexplib.Sexp.to_string (sexp_of_payouts_mach (Pay payouts))

let string_of_payouts = string_of_payouts_hum

let pretty_of_payouts payouts =
  let open Rule in
  let pay2 = match payouts with
    | [] -> [] | [p0] -> [p0]
    (* since Surrender has constant payout (-0.5), this case isn't interesting
     * unless it's the top strategy *)
    | p0::(Surrender, o1)::p2::tail -> [p0; p2]
    | p0::p1::tail -> [p0; p1] in
  let text = pay2
             |> map (fun (a, p) -> Printf.sprintf "%c%+.2f"
                        (char_of_action a) (float_of_num p))
             |> concat " " |> ljust 13 ' ' in
  let colorit p = concat "" [(assoc (fst p) c_open); text; c_close] in
  match pay2 with
  | [] -> text
  | p0::[] -> colorit p0
  | p0::p1::_ ->
    (* don't colour it in if there's not much difference *)
    if abs_num ((snd p0) -/ (snd p1)) <=/ eps then text
    else colorit p0

module Table (Sim: Sim.S) = struct
  let cols = [2;3;4;5;6;7;8;9;10;1]
  let col_div = " | "
  let row_div = concat "-+-" ("--" :: (map (fun _ -> make 13 '-') cols))
  let header =
    concat ""
      ["P\\H| "; concat col_div
         (map (fun h0 ->
              (make 1 (char_of_card (card_of_int h0))) |> rjust 13 ' ') cols)]

  let _house_play_and_payout m =
    let open Sim in
    m
    |> Prob.map Table.next_turn (* yes, unconditionally finish the turn *)
    |> exec_round (fun p -> assert (p = 0); Rule.house_play)
    |> payout_of_player Rule.pay 1

  let rec payout_of_hands m0 card_list =
    let open Rule in (* the main Rule module, which defines [type actions] *)
    let open Sim in
    let open Prob in
    let p0, h0, no_split = match card_list with
      | [pc0; hc0] -> new_hand ++ pc0, new_hand ++ hc0, true
      | [pc0; hc0; pc1] -> new_hand ++ pc0 ++ pc1, new_hand ++ hc0, pc0 <> pc1
      | _ -> failwith "not implemented" in
    let m1 = m0 |> deal_next_game card_list in
    let maybe_play a f = if not @@ mem a Rule.actions then [] else match f () with
        | None -> []
        | Some p -> [(a, p)] in
    let pay_u = maybe_play Surrender (fun () ->
        Some ((-1) /// 2)) in
    let pay_s = maybe_play Stand (fun () ->
        Some (_house_play_and_payout m1)) in
    let pay_h = maybe_play Hit (fun () ->
        if not @@ can_hit p0 then None else
          (* TODO: approx2h *)
          Some (m1 >>= Table.hit None |> _house_play_and_payout)) in
    let pay_d = maybe_play Double (fun () ->
        if pay_h = [] then None else
          Some ((num_of_int 2) */ (snd (hd pay_h)))) in
    let pay_p = maybe_play Split (fun () ->
        if no_split then None else
          let payout1 = payout_of_hands m0 [hd card_list; hd (tl card_list)] in
          Some ((num_of_int 2) */ (snd (hd payout1)))) in
    [pay_u; pay_s; pay_h; pay_d; pay_p]
    |> List.concat
    |> sort (fun pay0 pay1 -> - compare_num (snd pay0) (snd pay1))

  let print_payout fp m0 card_list =
    let p0, hc0 = match card_list with
      | [pc0; hc0; pc1] -> new_hand ++ pc0 ++ pc1, hc0
      | _ -> failwith "not implemented" in
    print_endline (
      string_of_hand p0 ^ " vs House " ^ make 1 (char_of_card hc0) ^ ": " ^
      string_of_payouts (payout_of_hands m0 card_list))

  let print_row fp m0 pc0 pc1 =
    let row_hd = BatString.of_list [char_of_card pc0; char_of_card pc1] in
    let print_string s = fprintf fp "%s%!" s in
    print_string row_hd;
    iter (fun hc0 ->
        print_string col_div;
        print_string (pretty_of_payouts (payout_of_hands m0 [pc0; (card_of_int hc0); pc1]));
      ) cols;
    fprintf fp "\n%!";;

  let print_all fp m0 =
    let print_row i0 i1 = print_row fp m0 (card_of_int i0) (card_of_int i1) in
    let print_endline s = fprintf fp "%s\n%!" s in
    print_endline header;
    print_endline row_div;
    iter (print_row 1) [10;9;8;7;6;5;4;3;2];
    print_endline row_div;
    iter (print_row 10) [9;8;7;6;5;4;3;2];
    iter (print_row 2) [9;8;7;6;5;4;3];
    print_endline row_div;
    iter (fun pc0 -> print_row pc0 pc0) [1;10;9;8;7;6;5;4;3;2];;

end
