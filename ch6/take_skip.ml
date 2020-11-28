let rec take x =
  match x with
  | [] -> []
  | x :: rest -> x :: skip rest
and skip y =
  match y with
  | [] -> []
  | y :: rest -> take rest
