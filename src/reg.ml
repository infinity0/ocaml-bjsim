(** Utilities for building string-mapped registries of modules. *)

type 'm empty = {
  all : (string * 'm) list;
  reg_add : (string * 'm) list -> string -> 'm -> (string * 'm) list
}

let reg_init l : 'm empty =
  let all = l in
  let reg_add l k v = (k, v) :: l in
  { all; reg_add; }

type 'm reg = {
  of_name : string -> 'm option;
  to_name : 'm -> string option;
  all_names : string list;
}

let reg_make all : 'm reg =
  let of_name s = try Some (List.assoc s all) with _ -> None in
  let to_name m = try Some (BatList.assq_inv m all) with _ -> None in
  let all_names = List.map fst all in
  { of_name; to_name; all_names; }
