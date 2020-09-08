let rec contains x l = 
  match l with
      [] -> false
    | y::rest -> 
        if x = y 
          then true 
          else contains x rest ;;

let rec containsNoRepeats l =
  match l with
      [] -> true
    | x::rest ->
        if contains x rest 
          then false 
          else containsNoRepeats rest ;;