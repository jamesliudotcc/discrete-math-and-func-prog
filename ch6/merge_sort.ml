let rec split1 (l, aa, bb) =
  match (l, aa, bb) with
  | ([], aa, bb )-> (aa, bb)
  | (x :: rest, aa, bb) -> split2 (rest, x :: aa, bb)
and split2 (l, aa, bb) =
  match (l, aa, bb) with
  | ([], aa, bb) -> (aa, bb)
  | (x :: rest, aa, bb) -> split1 (rest, aa, x :: bb)

let rec merge(aa, bb) =
  match (aa, bb) with
  | ([], bb) -> bb
  | (aa, []) -> aa
  | (a :: a_rest, b :: b_rest) ->
     if a < b
     then a::merge(a_rest, b::b_rest)
     else b::merge(a::a_rest, b_rest)

let rec merge_sort l =
  match l with
  | [] -> []
  | [a] -> [a]
  | xx ->
     let
       (aa, bb) = split1(xx, [], [])
     in merge(merge_sort(aa), merge_sort(bb))
