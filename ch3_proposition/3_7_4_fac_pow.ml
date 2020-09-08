let rec factorPow n i =
  if n mod i != 0 then 0
  else
    1 + factorPow (n / i) i
