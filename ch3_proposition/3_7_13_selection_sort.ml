let rec remove_first item a_list =
  match a_list with
  | [] -> []
  | head::rest ->
    if head = item then rest 
    else head::remove_first item rest ;;

let rec list_min list_of_int =
  match list_of_int with
  | head::[] -> head
  | head::rest ->
      if head < list_min rest then head
      else list_min rest ;;

let rec selection_sort list_to_sort =
  match list_to_sort with
  | [] -> []
  | _ -> 
      list_min list_to_sort::selection_sort (
        remove_first (list_min list_to_sort) list_to_sort
        ) ;;