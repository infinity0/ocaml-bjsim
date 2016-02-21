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

let (///) a b = num_of_int a // num_of_int b


open Reg
let { all; reg_add; } : (module S) empty = reg_init []


module VoidShoe = struct
  type t = unit

  let new_shoe n = ()

  let card_prob d = all_cards
                    |> map (fun c -> c, (num_per_deck c) /// num_all_per_deck)
                    |> Prob.dist_of_al

  let draw_card c d = d

  let draw = derive_draw draw_card card_prob

  let string_of_shoe d = "()"

  let shoe_of_string s = match s with
    | "()" -> new_shoe 0
    | _ -> raise (Invalid_argument "the only valid VoidShoe is ()")
end
let all = reg_add all "VoidShoe" (module VoidShoe)


module RealShoe = struct
  type t = {
    num_decks: int;
    num_dealt: int list;
  } with sexp

  let cards_dealt d = all_cards
                      |> map (fun c -> num_per_deck c * d.num_decks)
                      |> combine d.num_dealt |> combine all_cards

  let num_all_left d = num_all_per_deck * d.num_decks - (fold_left (+) 0 d.num_dealt)

  let check_t d =
    if d.num_decks <= 0 then
      raise (Invalid_argument "num_decks must be positive")
    else if length d.num_dealt != length all_cards then
      raise (Invalid_argument "num_dealt has bad length")
    else if not @@ for_all (fun (c, (n, t)) -> t - n >= 0) (cards_dealt d) then
      raise (Invalid_argument "too many cards dealt for a particular card")
    else
      d

  let new_shoe n = {
    num_decks = n;
    num_dealt = map (fun _ -> 0) all_cards;
  }

  let num_dealt s c = nth s.num_dealt (int_of_card c - 1)

  let card_prob d =
    let r = num_all_left d in
    cards_dealt d
    |> map (fun (c, (n, t)) -> c, (t - n) /// r)
    |> Prob.dist_of_al

  let draw_card card d = {
    d with
    num_dealt = map (fun (c, n) -> if c = card then n + 1 else n)
        (combine all_cards d.num_dealt);
  }

  let draw = derive_draw draw_card card_prob

  let string_of_shoe d = Sexplib.Sexp.to_string (sexp_of_t d)

  let shoe_of_string s = check_t (t_of_sexp (Sexplib.Sexp.of_string s))
end
let all = reg_add all "RealShoe" (module RealShoe)


let { of_name; to_name; all_names; } = reg_make all
