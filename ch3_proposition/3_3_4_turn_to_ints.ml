let boolToBinary input =
  match input with
    false -> 0
  | true -> 1 ;;

let rec turnToInts listOfBools =
  match listOfBools with
    [] -> []
  | head::rest -> boolToBinary head::turnToInts rest ;;