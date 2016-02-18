(** Rules for Blackjack and its many variations *)
open Hand
open Num

(** Actions possible in any game *)
type action = Hit | Stand | Double | Split | Surrender

(**
 * Module functions, as a record.
 *
 * The type parameter ['s] is the data type of the shoe on the table.
*)
type 's r = {
  name : string;
  (** Name of this game variant. *)

  actions : action list;
  (** List of actions allowed by this variant. *)

  default_num_decks : int;
  (** Default number of card decks to use. *)

  pay : hand -> hand -> num;
  (**
   * What to pay to the player.
   *
   * First argument is the house's hand, second argument is the player's hand.
  *)

  house_play : 's Table.t -> 's Table.t Prob.m;
  (**
   * Play the house's turn.
  *)
}

val pay_base :
  ?nat_payout:num ->
  ?house_bust:(hand -> num) -> hand -> hand -> num

(**
 * Blackjack, standard variant.
 *
 * Natural pays 3:2. Dealer must hit soft-17.
*)
val bj : 's Table.r -> 's r

(**
 * Blackjack Switch (Las Vegas).
 *
 * Cannot surrender. Natural pays 1:1. Dealer hits soft-17 and pushes on 22.
 * We do not (yet) simulate or estimate a strategy for switching.
*)
val bjs : 's Table.r -> 's r

(**
 * Blackjack, as seen on some of the video machines in Las Vegas.
 *
 * Can only hit/switch. Natural pays 1:1.
*)
val bjv : 's Table.r -> 's r
