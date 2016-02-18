open Hand
open List
open Num
open Sexplib.Std

type 's r = {
  (* TODO: deal with empty shoes *)
  new_shoe : int -> 's;
  card_prob : 's -> card Prob.m;
  num_dealt : 's -> card -> int;
  draw_card : card -> 's -> 's;
  draw : 's -> (card * 's) Prob.m;
  string_of_shoe : 's -> string;
  shoe_of_string : string -> 's;
}

let derive_draw draw_card card_prob = fun d ->
  Prob.map (fun c -> c, draw_card c d) (card_prob d)

let (///) a b = num_of_int(a) // num_of_int(b)


type voidShoe_t = unit

let voidShoe : voidShoe_t r =
  let new_shoe n = () in

  let card_prob d = all_cards
                    |> map (fun c -> c, (num_per_deck c) /// num_all_per_deck)
                    |> Prob.dist_of_al in

  let num_dealt c d = raise (Failure "not implemented") in

  let draw_card c d = d in

  let draw = derive_draw draw_card card_prob in

  let string_of_shoe d = "()" in

  let shoe_of_string s = match s with
    | "()" -> new_shoe 0
    | _ -> raise (Invalid_argument "the only valid VoidShoe is ()") in

  { new_shoe; card_prob; num_dealt; draw_card; draw; string_of_shoe; shoe_of_string; }


type realShoe_t = {
  num_decks: int;
  num_dealt: int list;
} with sexp

let realShoe : realShoe_t r =
  let cards_dealt d = all_cards
                      |> map (fun c -> num_per_deck c * d.num_decks)
                      |> combine d.num_dealt |> combine all_cards in

  let num_all_left d = num_all_per_deck * d.num_decks - (fold_left (+) 0 d.num_dealt) in

  let check_t d =
    if d.num_decks <= 0 then
      raise (Invalid_argument "num_decks must be positive")
    else if length d.num_dealt != length all_cards then
      raise (Invalid_argument "num_dealt has bad length")
    else if not @@ for_all (fun (c, (n, t)) -> t - n >= 0) (cards_dealt d) then
      raise (Invalid_argument "too many cards dealt for a particular card")
    else
      d in

  let new_shoe n = {
    num_decks = n;
    num_dealt = map (fun _ -> 0) all_cards;
  } in

  let num_dealt s c = nth s.num_dealt (int_of_card c - 1) in

  let card_prob d =
    let r = num_all_left d in
    cards_dealt d
    |> map (fun (c, (n, t)) -> c, (t - n) /// r)
    |> Prob.dist_of_al in

  let draw_card card d = {
    d with
    num_dealt = map (fun (c, n) -> if c = card then n + 1 else n)
        (combine all_cards d.num_dealt);
  } in

  let draw = derive_draw draw_card card_prob in

  let string_of_shoe d = Sexplib.Sexp.to_string (sexp_of_realShoe_t d) in

  let shoe_of_string s = check_t (realShoe_t_of_sexp (Sexplib.Sexp.of_string s)) in

  { new_shoe; card_prob; num_dealt; draw_card; draw; string_of_shoe; shoe_of_string; }
