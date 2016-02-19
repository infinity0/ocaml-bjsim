(** Rules for Blackjack and its many variations *)
open Hand
open Num

(** Actions possible in any game *)
type action = Hit | Stand | Double | Split | Surrender

val char_of_action : action -> char

(** Rules for a particular game variant. *)
module type S = sig
  (** Type of table used in this game. *)
  type table_t

  (** Name of this game variant. *)
  val name : string

  (** List of actions allowed by this variant. *)
  val actions : action list

  (** Default number of card decks to use. *)
  val default_num_decks : int

  (**
   * What to pay to the player.
   *
   * First argument is the house's hand, second argument is the player's hand.
  *)
  val pay : hand -> hand -> num

  (**
   * Play the house's turn.
  *)
  val house_play : table_t -> table_t Prob.m
end

module type MakeS = functor (Table: Table.S) -> S with type table_t = Table.t

val pay_base :
  ?nat_payout:num ->
  ?house_bust:(hand -> num) ->
  hand -> hand -> num

(**
 * Blackjack, standard variant.
 *
 * Natural pays 3:2. Dealer must hit soft-17.
*)
module BJ : MakeS

(**
 * Blackjack Switch (Las Vegas).
 *
 * Cannot surrender. Natural pays 1:1. Dealer hits soft-17 and pushes on 22.
 * We do not (yet) simulate or estimate a strategy for switching.
*)
module BJS : MakeS

(**
 * Blackjack, as seen on some of the video machines in Las Vegas.
 *
 * Can only hit/switch. Natural pays 1:1.
*)
module BJV : MakeS


val of_name : string -> (module MakeS) option
val to_name : (module MakeS) -> string option
val all_names : string list
