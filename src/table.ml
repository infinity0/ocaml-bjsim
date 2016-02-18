open Hand
open BatList
(*open List*)

type 's desc = {
  shoe: 's;
  hands_desc: string list;
  turn: int;
  finished: bool;
}

type 's t = {
  shoe: 's;
  hands: hand list;
  turn: int;
  finished: bool;
}

type 's r = {
  new_table : int -> 's -> 's t;
  num_players : 's t -> int;
  all_players : 's t -> int list;
  current_player : 's t -> int;
  hand_of_house : 's t -> hand;
  hand_of_player : int -> 's t -> hand;
  is_turn_finished : 's t -> bool;
  is_deal_complete : 's t -> bool;
  finish_turn : 's t -> 's t;
  next_turn : 's t -> 's t;
  next_turn_checked : 's t -> 's t;
  next_game : 's t -> 's t;
  hit : card option -> 's t -> 's t Prob.m;
  hit_or_finish : (hand -> bool) -> card option -> 's t -> 's t Prob.m;
  describe_hands : 's t -> 's desc;
}

let make shoe_r =
  let new_table players shoe = {
    shoe = shoe;
    hands = make (players + 1) new_hand;
    turn = players;
    finished = false;
  } in

  let num_players t = length t.hands - 1 in

  let all_players t = range (num_players t) `Downto 0 in

  let current_player t = t.turn in

  let hand_of_house t = hd t.hands in

  let hand_of_player i t = nth t.hands i in

  let is_turn_finished t = t.finished in

  let is_deal_complete t = for_all Hand.is_deal_complete t.hands in

  let finish_turn t = {
    t with
    finished = true;
  } in

  let next_turn t = {
    t with
    turn = if t.turn > 0 then t.turn - 1 else ((length t.hands) - 1);
    finished = false;
  } in

  let next_turn_checked t =
    if not t.finished then
      raise (Invalid_argument "turn not finished")
    else
      next_turn t in

  let next_game t = {
    t with
    hands = make (num_players t + 1) new_hand;
    turn = num_players t;
    finished = false;
  } in

  let _replace_nth n l e =
    let left, right = split_at n l in
    if is_empty right then
      raise (Invalid_argument "out-of-bounds")
    else
      left @ e :: (tl right) in

  let _set_hand t i h = _replace_nth i t.hands h in

  let hit co t =
    let open Shoe in let {
      draw; draw_card;
    } = shoe_r in
    let h = hand_of_player (current_player t) t in
    if not (can_hit h) then
      Prob.return t
    else
      let shoe_dist = match co with
        | None -> draw t.shoe
        | Some(c) -> Prob.return (c, draw_card c t.shoe) in
      Prob.map (fun (c, next_shoe) -> {
            t with
            shoe = next_shoe;
            hands = _set_hand t t.turn (h ++ c)
          }) shoe_dist in

  let hit_or_finish pred co t =
    let h = hand_of_player (current_player t) t in
    if can_hit h && pred h then
      hit co t
    else
      Prob.return (finish_turn t) in

  let describe_hands t : 's desc =
    let h0 = hd t.hands in
    let describe =
      if is_bust h0 then
        fun h -> if is_bust h then "==" else "++"
      else if is_nat h0 then
        fun h -> if is_bust h then "xx" else if is_nat h then "==" else "<<"
      else
        let v0 = int_of_hand h0 in
        fun h -> if is_bust h then "xx" else if is_nat h then "AJ" else
            let v = int_of_hand h in
            if v < v0 then "<<" else if v > v0 then ">>" else "==" in
    {
      shoe = t.shoe;
      hands_desc = string_of_hand (hd t.hands) :: map describe (tl t.hands);
      turn = t.turn;
      finished = t.finished;
    } in

  {
    new_table;
    num_players;
    all_players;
    current_player;
    hand_of_house;
    hand_of_player;
    is_turn_finished;
    is_deal_complete;
    finish_turn;
    next_turn;
    next_turn_checked;
    next_game;
    hit;
    hit_or_finish;
    describe_hands;
  }
