let mul (a, b) =
  let m x = a * x
  in m b

let mul_maker a =
  let m x = a * x
  in m

(* How to define an annonymous function *)
let annonymous = fun x -> (x + 3) mod 5


let mul_maker a = fun x -> a * x

let doubler = mul_maker 2

let rec partition (p, l, aa, bb, m) =
  match (p, l, aa, bb, m) with
  | (p, [], aa, bb, m) -> (aa, bb)
  | (p, x::rest, aa, bb, m) ->
     if m (x, p) then partition (p, rest, x::aa, bb, m)
     else partition (p, rest, aa, x::bb, m)

let rec quicksort (l, m) =
  match (l, m) with
  | ([], m) -> []
  | (x::rest, m) ->
     let (aa, bb) = partition(x, rest, [], [], m)
     in quicksort(aa, m) @ (x :: quicksort (bb, m))
;;

(* This quicksorts based on the function: *)

quicksort ([5; 2; 7; 4;  1; 9; 3], fun (a, b) -> a > b)


let fahr_to_cel fahr =
  (fahr -. 32.) *. 5. /. 9.

let make_list_extractor xx =
  let rec find_nth(l, x) =
    match (l, x) with
    | ([], x) -> -1
    | (a :: rest, 1) -> a
    | (a :: rest, x) -> find_nth (rest, x - 1)
  in
  fun x -> find_nth (xx, x)

let make_list_mul x =
  let rec list_mul l =
    match l with
    | [] -> []
    | a :: rest -> a * x :: list_mul rest
  in
  list_mul

let rec filter fn l =
  match l with
  | [] -> []
  | a :: rest ->
     if fn a
     then a :: filter fn rest
     else filter fn rest
;;

(* Example with anonymous function*)

filter (fun x -> x mod 2 = 0) [3;4;5];;
