let n_div_d n d =
  let rec find_q_r q r =
    if r < d
    then q, r
    else find_q_r (q + 1) (r - d)
  in find_q_r 0 n
