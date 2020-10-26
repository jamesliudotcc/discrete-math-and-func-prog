let x_to_y_power x y =
  let rec a_times_b_to_n a b n =
    match a, b, n with
    | a, b, 0 -> a
    | a, b, n ->
      if a mod 2 = 0
      then b * a_times_b_to_n a b (n / 2)
      else  a * b * a_times_b_to_n a b (n - 1)
  in a_times_b_to_n 1 x y
