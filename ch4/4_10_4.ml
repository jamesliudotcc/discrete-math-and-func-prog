let mul x y =
  let rec mul_by_addition a b c =
    match a, b, c with
    | a, b, 0 -> a
    | a, b, c ->
       if a mod 2 = 0
       then b + mul_by_addition a (b+b) (c / 2) (* Why doesn't this work?*)
       else a + b + mul_by_addition a b (c-1)
  in mul_by_addition 0 x y
