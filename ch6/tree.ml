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
