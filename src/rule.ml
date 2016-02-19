open Num
open Hand

type action = Hit | Stand | Double | Split | Surrender

let char_of_action a = match a with
  | Hit -> 'H' | Stand -> 'S' | Double -> 'D' | Split -> 'P' | Surrender -> 'U'

module type S = sig
  type table_t
  val name : string
  val actions : action list
  val default_num_decks : int
  val pay : hand -> hand -> num
  val house_play : table_t -> table_t Prob.m
end

module type MakeS = functor (Table: Table.S) -> S with type table_t = Table.t


let _neg_one = num_of_int (-1)
let _zero = num_of_int 0
let _one = num_of_int 1

module HouseHitA17 (Table : Table.S) = struct
  let house_play t =
    Table.hit_or_finish (fun h -> int_of_hand h <= 16 || is_a17 h) None t
end

module HouseStandA17 (Table : Table.S) = struct
  let house_play t =
    Table.hit_or_finish (fun h -> int_of_hand h <= 16) None t
end

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


open Reg
let { all; reg_add; } : (module MakeS) empty = reg_init []


module BJ (Table : Table.S) : S with type table_t = Table.t = struct
  type table_t = Table.t

  let name = "BlackJack standard"
  let actions = [ Hit; Stand; Double; Split; Surrender ]
  let default_num_decks = 8

  let pay h p = pay_base h p
  include HouseHitA17(Table)
end
let all = reg_add all "BJ" (module BJ)


module BJS (Table : Table.S) : S with type table_t = Table.t = struct
  type table_t = Table.t

  let name = "BlackJack Switch"
  let actions = [ Hit; Stand; Double; Split ]
  let default_num_decks = 8

  let _house_push_22 h = if is_22 h then _zero else _one
  let pay h p = pay_base ~nat_payout:_one ~house_bust:_house_push_22 h p
  include HouseHitA17(Table)
end
let all = reg_add all "BJS" (module BJS)


module BJV (Table : Table.S) : S with type table_t = Table.t = struct
  type table_t = Table.t

  let name = "BlackJack on the video machines"
  let actions = [ Hit; Stand ]
  let default_num_decks = 2

  let pay h p = pay_base ~nat_payout:_one h p
  include HouseStandA17(Table)
end
let all = reg_add all "BJV" (module BJV)


let { of_name; to_name; all_names; } = reg_make all;;
