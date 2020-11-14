(*Ties evaluate to false, useful for stable sorting*)
let rec should_come_after (first_list, second_list) =
  match (first_list, second_list) with
  | ([], _::_) -> false
  | (_::_, []) -> true
  | (head1::tail1, head2::tail2) ->
     if head1 > head2 then true
     else if head2 > head1 then false
     else should_come_after (tail1, tail2)
  | ([], []) -> false
