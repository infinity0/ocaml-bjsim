open Num
open Hand

type action = Hit | Stand | Double | Split | Surrender

type 's r = {
  name : string;
  actions : action list;
  default_num_decks : int;
  pay : hand -> hand -> num;
  house_play : 's Table.t -> 's Table.t Prob.m;
}


let _neg_one = num_of_int (-1)
let _zero = num_of_int 0
let _one = num_of_int 1

let house_hit_a17 table_r t =
  table_r.Table.hit_or_finish (fun h -> int_of_hand h <= 16 || is_a17 h) None t

let house_stand_a17 table_r t =
  table_r.Table.hit_or_finish (fun h -> int_of_hand h <= 16) None t

let pay_base
    ?(nat_payout = num_of_int 3 // num_of_int 2)
    ?(house_bust = fun _ -> _one)
    h p =
  let hs = status_of_hand h in
  let ps = status_of_hand p in
  match (hs, ps) with
  | (_, Bust) -> _neg_one
  | (Nat, Nat) -> _zero
  | (_, Nat) -> nat_payout
  | (Bust, Val _) -> house_bust h
  | (Nat, Val _) -> _neg_one
  | (Val hv, Val pv) -> num_of_int (compare pv hv)


let bj table_r =
  let name = "BlackJack standard" in
  let actions = [ Hit; Stand; Double; Split; Surrender ] in
  let default_num_decks = 8 in
  let pay h p = pay_base h p in
  let house_play = house_hit_a17 table_r in
  { name; actions; default_num_decks; pay; house_play; }


let bjs table_r =
  let name = "BlackJack Switch" in
  let actions = [ Hit; Stand; Double; Split ] in
  let default_num_decks = 8 in
  let _house_push_22 h = if is_22 h then _zero else _one in
  let pay h p = pay_base ~nat_payout:_one ~house_bust:_house_push_22 h p in
  let house_play = house_hit_a17 table_r in
  { name; actions; default_num_decks; pay; house_play; }


let bjv table_r =
  let name = "BlackJack on the video machines" in
  let actions = [ Hit; Stand ] in
  let default_num_decks = 2 in
  let pay h p = pay_base ~nat_payout:_one h p in
  let house_play = house_stand_a17 table_r in
  { name; actions; default_num_decks; pay; house_play; }
