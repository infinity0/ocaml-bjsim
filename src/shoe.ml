open Hand
open List
open Num
open Sexplib.Std

module type S = sig
  type t
  (* TODO: deal with empty shoes *)
  val new_shoe : int -> t
  val card_prob : t -> card Prob.m
  val draw_card : card -> t -> t
  val draw : t -> (card * t) Prob.m
  val string_of_shoe : t -> string
  val shoe_of_string : string -> t
end

let derive_draw draw_card card_prob = fun d ->
  Prob.map (fun c -> c, draw_card c d) (card_prob d)

let (/-) a b = num_of_int(a) // num_of_int(b)

module VoidShoe = struct
  type t = unit

  let new_shoe n = ()

  let card_prob d = all_cards
                    |> map (fun c -> c, (cards_per_deck c) /- 52)
                    |> Prob.dist_of_al

  let draw_card c d = d

  let draw = derive_draw draw_card card_prob

  let string_of_shoe d = "()"

  let shoe_of_string s = new_shoe 0
end

module RealShoe = struct
  type t = {
    num_decks: int;
    num_dealt: int list;
  } with sexp

  let cards_dealt d = combine all_cards d.num_dealt

  let new_shoe n = {
    num_decks = n;
    num_dealt = map (fun _ -> 0) all_cards;
  }

  let card_prob d =
    let cards_left = 52 * d.num_decks - (fold_left (+) 0 d.num_dealt) in
    cards_dealt d
    |> map (fun (c, dealt) ->
        c, (cards_per_deck c * d.num_decks - dealt) /- cards_left)
    |> Prob.dist_of_al

  let draw_card card d = {
    d with
    num_dealt = map (fun (c, n) -> if c = card then n + 1 else n)
        (cards_dealt d);
  }

  let draw = derive_draw draw_card card_prob

  let string_of_shoe d = string_of_sexp (sexp_of_t d)

  let shoe_of_string s = t_of_sexp (sexp_of_string s)
end
