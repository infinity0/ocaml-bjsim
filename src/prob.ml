open Num
open List
open Sexplib.Std
open Sexplib_num.Std.Num

type 'a m = ProbDist of ('a * num) list with sexp

exception Invalid_probability of num
exception Invalid_total_probability of num

let _zero = num_of_int 0
let _one = num_of_int 1

let _foldp dist f = fold_left f _zero (map snd dist)

let _unchecked_dist_of_al checked_dist =
  let open BatHashtbl in
  checked_dist
  |> fold_left (fun t (v, p) ->
      modify_def _zero v ((+/) p) t; t
    ) (create (List.length checked_dist))
  |> fold (fun k v l -> (k, v) :: l)
  |> fun fold_th -> ProbDist (fold_th [])

let dist_of_al dist =
  (* ensure non-negative *)
  let min = (_foldp dist min_num) in
  if min </ _zero then
    raise (Invalid_probability min)
  else
    (* ensure sum to one *)
    let sum = (_foldp dist add_num) in
    if sum <>/ _one then
      raise (Invalid_total_probability sum)
    else
      (* filter out zero-probability events *)
      dist
      |> filter (fun (v, p) -> p <>/ _zero)
      |> _unchecked_dist_of_al

let al_of_dist m = match m with ProbDist(dist) -> dist

let fal_of_dist m = map (fun (v, p) -> v, float_of_num(p)) (al_of_dist m)

let string_of_dist sexp_of_v m = Sexplib.Sexp.to_string (sexp_of_m sexp_of_v m)

let given pred m =
  let dist = filter (fun (v, p) -> pred(v)) (al_of_dist m) in
  let sum = (_foldp dist add_num) in
  if sum =/ _zero then
    None
  else
    Some (sum, dist_of_al (map (fun (v, p) -> (v, p // sum)) dist))

let expect_a add mult m =
  let l = map (fun (v, p) -> mult v p) (al_of_dist m) in
  fold_left add (hd l) (tl l)

let expect f m = expect_a add_num (fun v p -> mult_num (f v) p) m

let certain m = let al = al_of_dist m in
  if length al = 1 then Some (fst (hd al)) else None

(* generic monad/structural helpers below *)

let return v = ProbDist [(v, _one)]

let bind m f =
  let al_list =
    map (fun (v0, p0) ->
        map (fun (v1, p1) -> v1, p1 */ p0)
          (al_of_dist (f v0)))
      (al_of_dist m) in
  _unchecked_dist_of_al (concat al_list)
let (>>=) = bind

let join m = m >>= (fun x -> x)

let apply fm am = fm >>= (fun f -> am >>= (fun a -> return (f a)))
let (<*>) = apply

let map f m = al_of_dist m |> map (fun (v, p) -> f v, p) |> _unchecked_dist_of_al
(* more general but slower: m >>= fun v -> return (f v) *)
let (<$>) = map

let filter pred m =
  snd (BatOption.get_exn (given pred m)
         (Invalid_total_probability _zero))

let exists pred m = m |> al_of_dist |> List.map fst |> exists pred
(* equiv but slower: match certain (map pred m) with Some false -> false | _ -> true *)

let for_all pred m = m |> al_of_dist |> List.map fst |> for_all pred
(* equiv but slower: match certain (map pred m) with Some true -> true | _ -> false *)
