let isEven x =
  x mod 2 = 0 ;;

let rec areEven listOfInts =
  match listOfInts with
      [] -> []
    | head::rest -> isEven head :: areEven rest;;