let rec all_even l =
  match l with
  | [] -> true
  | head::tail ->
      if head mod 2 != 0 then false
      else all_even tail ;;

let rec has_even l =
  match l with
  | [] -> false
  | head::tail ->
      if head mod 2 = 0 then true
      else has_even tail ;;

let rec not_all_even l =
  match l with
  | [] -> false
  | head::tail ->
      if head mod 2 != 0 then true
      else not_all_even tail ;;

let rec none_even l =
  match l with
  | [] -> true
  | head::tail -> 
      if head mod 2 == 0 then false
      else none_even tail ;;