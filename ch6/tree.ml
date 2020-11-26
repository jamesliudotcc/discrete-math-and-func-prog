type tree = Leaf of int | Internal of (int * tree * tree)

let rec tree_sum x =
  match x with
  | Leaf(x) -> x
  | Internal(x, left, right) ->
     x + tree_sum left + tree_sum right

(* When translating SML to OCaml, drop the name of the function in the match with
part of the expression, since that is already in the let, and match just takes the
part with the arguments *)

let rec print_tree x =
  match x with
  | Leaf(x) -> string_of_int x
  | Internal(x, left, right) -> "(" ^ string_of_int x ^ ": "
                                ^ print_tree left ^ ", "
                                ^ print_tree right ^ ")"

let rec nodes x =
  match x with
  | Leaf(x) -> 1
  | Internal(x, left, right) ->
     1 + nodes left + nodes right

let rec max t =
  match t with
  | Leaf(t) -> t
  | Internal(t, left, right) ->
     if t > max left && t > max right
     then t
     else
       if max left > max right
       then max left
       else max right

let rec min t =
  match t with
  | Leaf(t) -> t
  | Internal(t, left, right) ->
     if t < min left && t < min right
     then t
     else
       if min left < min right
       then min left
       else min right

let rec is_in_order t =
  match t with
  | Leaf(t) -> true
  | Internal(t, left, right) ->
     if is_in_order left && is_in_order right
     then max left <= t && t <= min right
     else false
