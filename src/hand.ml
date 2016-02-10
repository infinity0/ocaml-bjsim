type card = Card of int

let card_of_int i =
  if 0 < i && i <= 10 then
    Card i
  else
    raise (Invalid_argument (string_of_int i))

let int_of_card c = match c with Card i -> i

let string_of_card c = match c with
  | Card 1 -> "A"
  | Card 10 -> "J"
  | Card i -> string_of_int(i)

let string_of_card_opt co = match co with
  | Some c -> string_of_card c
  | None -> "?"

let all_cards = List.map card_of_int [1; 2; 3; 4; 5; 6; 7; 8; 9; 10]

let cards_per_deck c = match (int_of_card c) with
  | 10 -> 16
  | _ -> 4


type hand = {
  ace: bool;
  osum: int;
  fst: card option;
  snd: card option;
}

type hand_status = Bust | Nat | Val of int

let new_hand = {
  ace = false;
  osum = 0;
  fst = None;
  snd = None;
}

let cards_dealt h = match h with
  | { snd = Some _; _ } -> 2
  | { fst = Some _; _ } -> 1
  | { ace = false; osum = 0; _ } -> 0
  | _ -> 3

let is_deal_complete h = (cards_dealt h) >= 2

let int_of_hand h =
  if not h.ace then
    h.osum
  else if h.osum >= 11 then
    h.osum + 1
  else
    h.osum + 11

let is_bust h = if h.ace then h.osum >= 21 else h.osum >= 22

let is_nat h = h.ace && h.osum = 10 && cards_dealt h == 2

let status_of_hand h =
  (* if is_bust h then Bust else if is_nat h then Nat else Val (int_of_hand h) *)
  if h.ace then
    if h.osum >= 21 then Bust
    else if h.osum >= 11 then Val (h.osum + 1)
    else if h.osum = 10 && h.snd != None then Nat
    else Val (h.osum + 11)
  else
  if h.osum >= 22 then Bust else Val h.osum

let is_a17 h = h.ace && h.osum = 6

let is_22 h = if h.ace then h.osum = 11 || h.osum = 21 else h.osum = 22

let can_hit h = not (is_nat h) && not (is_bust h)

let (++) h c =
  let i = int_of_card c in
  let (ace, osum) =
    if i = 1 && not h.ace then
      true, h.osum
    else
      h.ace, h.osum + i in
  let num = cards_dealt h in
  let (fst, snd) = match num with
    | 0 -> Some c, None
    | 1 -> h.fst, Some c
    | _ -> None, None in
  { ace = ace; osum = osum; fst = fst; snd = snd }

let string_of_hand h =
  if is_bust h then
    "xx"
  else if is_nat h then
    "AJ"
  else
    let maybe_ace = if h.ace then "A" else "" in
    let orig_cards =
      if cards_dealt h <= 2 then
        "(" ^ (string_of_card_opt h.fst) ^ (string_of_card_opt h.snd) ^ ")"
      else
        "" in
    maybe_ace ^ (string_of_int h.osum) ^ orig_cards

let hand_of_card_list l = List.fold_left (++) new_hand l
