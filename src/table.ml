open Hand
open BatList
(*open List*)

module type S = sig
  type shoe_t
  type t
  val new_table : int -> shoe_t -> t
  val num_players : t -> int
  val all_players : t -> int list
  val current_player : t -> int
  val hand_of_house : t -> hand
  val hand_of_player : int -> t -> hand
  val is_turn_finished : t -> bool
  val is_deal_complete : t -> bool
  val finish_turn : t -> t
  val next_turn : t -> t
  val next_turn_checked : t -> t
  val next_game : t -> t
  val hit : card option -> t -> t Prob.m
  val hit_or_finish : (hand -> bool) -> card option -> t -> t Prob.m
end

type 'a default_t = {
  shoe: 'a;
  hands: hand list;
  turn: int;
  finished: bool;
}

module Make (Shoe: Shoe.S) :
  S with type shoe_t = Shoe.t and type t = Shoe.t default_t = struct
  type shoe_t = Shoe.t

  type t = shoe_t default_t

  let new_table players shoe = {
    shoe = shoe;
    hands = make (players + 1) new_hand;
    turn = players;
    finished = false;
  }

  let num_players t = length t.hands - 1

  let all_players t = range (num_players t) `Downto 0

  let current_player t = t.turn

  let hand_of_house t = hd t.hands

  let hand_of_player i t = nth t.hands i

  let is_turn_finished t = t.finished

  let is_deal_complete t = for_all Hand.is_deal_complete t.hands

  let finish_turn t = {
    t with
    finished = true;
  }

  let next_turn t = {
    t with
    turn = if t.turn > 0 then t.turn - 1 else ((length t.hands) - 1);
    finished = false;
  }

  let next_turn_checked t =
    if not t.finished then
      raise (Invalid_argument "turn not finished")
    else
      next_turn t

  let next_game t = {
    t with
    hands = make (num_players t + 1) new_hand;
    turn = num_players t;
    finished = false;
  }

  let _replace_nth n l e =
    let left, right = split_at n l in
    if is_empty right then
      raise (Invalid_argument "out-of-bounds")
    else
      left @ e :: (tl right)

  let _set_hand t i h = _replace_nth i t.hands h

  let hit co t =
    let h = hand_of_player (current_player t) t in
    if not (can_hit h) then
      Prob.return t
    else
      let shoe_dist = match co with
        | None -> Shoe.draw t.shoe
        | Some(c) -> Prob.return (c, Shoe.draw_card c t.shoe) in
      Prob.map (fun (c, next_shoe) -> {
            t with
            shoe = next_shoe;
            hands = _set_hand t t.turn (h ++ c)
          }) shoe_dist

  let hit_or_finish pred co t =
    let h = hand_of_player (current_player t) t in
    if can_hit h && pred h then
      hit co t
    else
      Prob.return (finish_turn t)

  type t_desc = {
    shoe: shoe_t;
    hands_desc: string list;
    turn: int;
    finished: bool;
  }

  let describe_hands t =
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
    }
end
