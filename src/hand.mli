(** Data structures to represent a card and a hand *)


(** A single card *)
type card

(**
 * Create a card out of an integer.
 *
 * 1 is ace, 10 is ten/jack/queen/king, values in between represent the
 * corresponding card, and all other values raise Invalid_argument.
 *)
val card_of_int : int -> card

val int_of_card : card -> int

val string_of_card : card -> string

val string_of_card_opt : card option -> string

val all_cards : card list

val cards_per_deck : card -> int


(** A single player's entire hand *)
type hand
type hand_status = Bust | Nat | Val of int

val new_hand : hand

val cards_dealt : hand -> int

val is_deal_complete : hand -> bool

val status_of_hand : hand -> hand_status

(**
 * Get the value of the hand. If there is an ace, the best value is returned.
 *)
val int_of_hand : hand -> int

val is_bust : hand -> bool

val is_nat : hand -> bool

val is_a17 : hand -> bool

val is_22 : hand -> bool

val can_hit : hand -> bool

val ( ++ ) : hand -> card -> hand

val string_of_hand : hand -> string

val hand_of_card_list : card list -> hand
