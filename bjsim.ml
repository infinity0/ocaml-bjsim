open Hand
open Cmdliner

(* has a minor bug in --help output; see dbuenzli/cmdliner#46 *)
let last_flag ?docs ?(doc = "") ?env names =
  let open Arg in
  let bool_parse s = match String.lowercase s with
    | "" | "false" | "f" | "no" | "n" | "0" -> `Ok false
    | "true" | "t" | "yes" | "y" | "1" -> `Ok true
    | s -> (fst bool) s in
  last &
  opt_all ~vopt:true (bool_parse, snd bool) [false] &
  info ?docs ~docv:"BOOL" ~doc ?env names

let converter_of_registry of_name to_name all_names =
  let of_name s = match of_name s with
    | Some m -> `Ok m
    | None -> `Error ("not found: " ^ s ^ "; choices are: " ^
                      (String.concat ", " all_names)) in
  let to_name m = match to_name m with
    | Some s -> s
    | None -> raise Not_found in
  of_name, fun st m -> Format.pp_print_string st (to_name m)

let rule_conv = converter_of_registry Rule.of_name Rule.to_name Rule.all_names

let shoe_conv = converter_of_registry Shoe.of_name Shoe.to_name Shoe.all_names

type shoe_spec = DefaultNumDecks | NumDecks of int | StringSpec of string

let shoe_spec_conv =
  let shoe_spec s = `Ok (match s with
    | "" | "0" -> DefaultNumDecks
    | _ -> try NumDecks (int_of_string s) with _ -> StringSpec s) in
  let to_name spec = match spec with
    | StringSpec s -> s
    | NumDecks i -> string_of_int i
    | DefaultNumDecks -> "0" in
  shoe_spec, fun st spec -> Format.pp_print_string st (to_name spec)

let hand_conv =
  let initial_cards s = match String.length s with
    | 3 -> `Ok (List.map card_of_char (BatString.to_list s))
    | _ -> `Error "each HAND must be 3 chars long" in
  let to_name cl = BatString.of_list (List.map char_of_card cl) in
  initial_cards, fun st cl -> Format.pp_print_string st (to_name cl)

let bjsim_main
    (rule:(module Rule.MakeS))
    (shoe_model:(module Shoe.S))
    (shoe_spec:shoe_spec)
    (approx2h:bool)
    (verbose:bool)
    (repl:bool)
    (hands:card list list) =
  let module OurShoe = (val shoe_model) in
  let module MakeOurRule = (val rule) in
  let module OurSim = Sim.Make2 (MakeOurRule) (OurShoe) in
  let sim = match shoe_spec with
    | DefaultNumDecks -> Lazy.force OurSim.new_sim
    | NumDecks i -> OurSim.new_sim_with_num_decks 1 i
    | StringSpec s -> OurSim.new_sim_with_shoe 1 (OurShoe.shoe_of_string s) in
  (* TODO: something with sim *)
  print_string ("approx2h=" ^ string_of_bool approx2h ^ "\n");
  print_string ("verbose =" ^ string_of_bool verbose ^ "\n");
  print_string ("repl    =" ^ string_of_bool repl ^ "\n");;

let cmd =
  let cmdname = "bjsim" in
  let version = "0.0" in
  let doc = "Calculate optimal strategies for various blackjack games." in
  let man = [
    `S "DESCRIPTION";
    `P "See README for more details.";
    (*`P "\
If calculations take too long, you can try setting `--prob-event-tolerance 1e-6 \
--prob-event-tolerance 1e-2` which shouldn't affect results much. If you only \
want to get a \"general sense\" of what a strategy looks like, you can try \
`--prob-event-tolerance 1e-4 --prob-event-tolerance 1e-1` but a few results \
will be visibly different from the actual optimal ones. \
";*)
    `S "NOTES";
    `P "In these docs, the variable $(i,BOOL) can take the values {true,t,yes,y,1} to mean true, or {,false,f,no,n,0} to mean false.";
    `S "EXAMPLES";
    `Pre "\
# Strategy table for standard Blackjack, no card counting.
\\[u0024] $(mname)

# Strategy table for Blackjack Switch. Partial AJHL card counting, half Js available.
\\[u0024] $(mname) --rule BJS --shoe-model AJHLShoe --shoe-init ((num_decks 6) (num_dealt (0 0 0 48)))

# Strategy table for Blackjack on the video machines. Perfect card counting.
\\[u0024] $(mname) --rule BJV --shoe-model RealShoe

# Exact payout for (10, 6) vs House 8, for standard Blackjack.
\\[u0024] $(mname) --shoe-model RealShoe J86
";
  ] in
  let ($) = Term.($) in
  let open Arg in
  let term =
    Term.const bjsim_main
    $ (value & opt rule_conv (module Rule.BJ) &
       info ["rule"]
         ~doc:"Blackjack rule to play with.")
    $ (value & opt shoe_conv (module Shoe.VoidShoe) &
       info ["shoe-model"]
         ~doc:"Which probabilistic model to use for the card shoe.")
    $ (value & opt shoe_spec_conv (DefaultNumDecks) &
       info ["shoe-spec"; "shoe-decks"; "shoe-init"]
         (* TODO: restrict certain args to certain flags only, e.g. --shoe-decks <int> *)
         ~doc:"\
Number of (empty) decks; or a sexp that describes the initial state. Default:
some number of empty decks, that is the default for the rule being played.")
    $ (last_flag ["approx2h"]
         ~doc:"Use 2nd-order calculation, which is slightly more accurate but much more expensive to calculate.")
    $ (last_flag ["verbose"]
         ~doc:"Show more output.")
    $ (last_flag ["repl"]
         ~doc:"Drop to the ocaml REPL after calculations are done.")
    $ (value & pos_all hand_conv [] &
       info [] ~docv:"HAND"
         ~doc: "\
3-char string describing the initial hand to calculate payout for. Each char is
a digit for the card with that face value, except J means 10/J/Q/K and A means
Ace. Cards are given in the order of being dealt, i.e. [player, house, player].
If omitted, we calculate optimal strategies for a representative set of initial
hands.")
  in
  term, Term.info cmdname ~version ~doc ~man

let () = match Term.eval cmd with
  | `Error _ -> exit 1
  | _ -> exit 0;;
