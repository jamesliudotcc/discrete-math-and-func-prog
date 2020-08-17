let rec factorial(n) = match n with
  0 -> 1
| n -> n * factorial (n -1);;
