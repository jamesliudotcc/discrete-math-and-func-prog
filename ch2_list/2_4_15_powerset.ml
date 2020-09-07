let rec addToEach x existing =
  match existing with
    [] -> []
  | (head::rest) -> (x::head)::(addToEach x rest) ;;

let rec powerSet it = 
  match it with
    [] -> [[]]
  | (head::rest) -> (addToEach head (powerSet rest))@powerSet rest ;;