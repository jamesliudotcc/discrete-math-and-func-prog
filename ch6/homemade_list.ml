type homemade_list = Null | Cons of int * homemade_list


(* Sigh, I suppose these errors are intentional accoridng to the textbook *)
let head l =
  match l with
    Cons(h, _) -> h

let tail l =
  match l with
    Cons(_, t) -> t

let rec concat (l1, l2) =
  match l1 with
  | Null -> l2
  | Cons(h, t) -> Cons (h, concat (t, l2))

let rec contains (n, l) =
  match l with
  | Null -> false
  | Cons (h, t) -> if h = n
                   then true
                   else contains (n, t)

let rec make_no_repeats l =
  match l with
  | Null -> Null
  | Cons (h, t) -> if contains (h, t)
                   then t
                   else Cons (h, t)

let rec sum l =
  match l with
  | Null -> 0
  | Cons (h, t) -> h + sum t
