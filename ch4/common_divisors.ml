let common_divisors a b =
  let rec com_div n =
    match n with
    | 1 -> [1]
    | n -> if a mod n == 0 && b mod n == 0
           then n::com_div(n-1)
           else com_div(n-1)
  in com_div(if a < b then a else b)
(* Do the recursive common divisor finding from smaller of a or b.
The purpose of the let block is to apply the recursive function
The purpose of the in is to do the choosing of the smaller of a or b.*)


let rec greatest l =
  match l with
  | [n] -> n
  | head::rest -> let z = greatest rest
                  in if head > z then head else z


let gcd a b = common_divisors a b |> greatest
