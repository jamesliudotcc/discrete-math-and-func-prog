let rec partition (p, l, aa, bb) =
  match (p, l, aa, bb) with
  | (p, [], aa, bb) -> (aa, bb)
  | (p, x :: rest, aa, bb) ->
     if x < p
     then partition (p, rest, x :: aa, bb)
     else partition (p, rest, aa, x :: bb)

let rec quick_sort l =
  match l with
  | [] -> []
  | x :: rest ->
     let (aa, bb) = partition (x, rest, [], [])
             in quick_sort aa @ (x :: quick_sort bb)
