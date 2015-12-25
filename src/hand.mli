(** Data structures to represent a card and a hand *)


(** A single card *)
type card

val card_of_int : int -> card

val int_of_card : card -> int

val string_of_card : card -> string

val string_of_card_opt : card option -> string

val all_cards : card list

val cards_per_deck : card -> int


(** A single player's entire hand *)
type hand

val new_hand : hand

val int_of_hand : hand -> int

val cards_dealt : hand -> int

val is_deal_complete : hand -> bool

val is_bust : hand -> bool

val is_nat : hand -> bool

val is_a17 : hand -> bool

val is_22 : hand -> bool

val can_hit : hand -> bool

val ( ++ ) : hand -> card -> hand

val string_of_hand : hand -> string
