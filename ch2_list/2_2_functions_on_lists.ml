let moveTwo xx = List.tl(List.tl(xx))@[List.hd(xx); List.hd(List.tl(xx))];;

let moveTwo2 (a::b::rest) = rest@[a;b];;

(* or *)

let moveTwo2 lst =
  match lst with
    a::b::rest -> rest@[a;b];;


let rec repeatEach lst =
  match lst with
    [] -> []
  | a::rest -> a::a::repeatEach(rest);;

  let rec sum(lst) =
    match lst with
      [] -> 0
    | x::rest -> x + sum(rest);;
  
    (* if it complains about a syntax error at ->, check if there is a | *)

let rec sumProduct lst =
  match lst with
    [] -> (0,1)
  | (x::rest) -> 
      let (s, p) = sumProduct(rest)
      in (x + s) , (x * p);;

    (* let - in syntax? *)
    (* From Ocaml tutorial: The standard phrase let name = expression in is used to define a named local expression, and name can then be used later on in the function instead of expression, till a ;; which ends the block of code. Notice that we don't indent after the in. Just think of let ... in as if it were a statement. *)

(* 2.2.3 *)
let rec doubler lst = 
  match lst with
    [] -> []
  | (x::rest) -> (2 * x)::doubler(rest);;

(* 2.2.4 *)
let rec count lst =
  match lst with
    [] -> 0
  | (x::rest) -> 1 + count(rest);;

(* 2.2.5 *)
let findNth (lst, n) =
  let rec findNthRec lst index =
    match lst with
    | [] -> -1
    | h::t when index = n -> h
    | _::t -> findNthRec t (index + 1)
  in findNthRec lst 1 ;;