let rec addToEach x existing =
  match existing with
    [] -> []
  | (head::rest) -> (x::head)::(addToEach x rest) ;;