let rec containsOne listOfInts =
  match listOfInts with
      [] -> false
    | 1::rest -> true
    | _::rest -> containsOne rest