type whole_number = Zero | OnePlus of whole_number

let succ(num) = OnePlus(num)

let rec as_whole_number n =
  match n with
  | 0 -> Zero
  | n -> OnePlus(as_whole_number(n-1))

let pred(OnePlus(num)) = num

let rec plus (x, y) =
  match (x, y) with
    (Zero, y) -> y
  | (x, Zero) -> x
  | (x, OnePlus(y)) -> (plus(OnePlus(x), y))

let rec is_less_than (x, y) =
  match (x, y) with
    (x, Zero) -> false
  | (Zero, y) -> true
  | (OnePlus(x), OnePlus(y)) -> is_less_than(x, y)

let rec minus (x, y) =
  match (x, y) with
  | (x, Zero) -> x
  | (OnePlus(x), OnePlus(y)) -> minus(x,y)

let rec as_int x =
  match x with
  | Zero -> 0
  | OnePlus(x) -> 1 + as_int(x)

let rec multiply (x, y) =
  match (x, y) with
  | (x, Zero) -> Zero
  | (Zero, y) -> Zero
  | (x, OnePlus(y)) -> plus(x, multiply(x, y))

let rec divide (x, y) = (* x / y *)
  if is_less_than(x, y)
  then Zero
  else OnePlus(divide(minus(x, y), y))
