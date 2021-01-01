type binary_logical_operation = Conjunction | Disjunction
type proposition = Var of string
                 | Negative of proposition
                 | BinaryOper of
                     binary_logical_operation
                     * proposition
                     * proposition

let conj(p, q) = BinaryOper(Conjunction, p, q)
let disj(p, q) = BinaryOper(Disjunction, p, q)
let cond(p, q) = BinaryOper(Disjunction, Negative(p), q)

(* Unicode literals work just fine on my machine*)
(* This didn't seem all that difficult to translate *)
(* Learned that nesting matching is a thing *)
let rec display argument =
  match argument with
  | Var(s) -> s
  | Negative(Var(s)) -> "¬" ^ s
  | Negative(p) -> "¬(" ^ display(p) ^ ")"
  | BinaryOper(oper, p, q) ->
     (match p with
      | BinaryOper(sub_op, _, _) -> if sub_op = oper
                                    then display(p)
                                    else "(" ^ display(p) ^ ")"
      | _ -> display(p)) ^
     (match oper with
      | Conjunction -> "∧"
      | Disjunction -> "∨") ^
     (match q with
      | BinaryOper(sub_op, _, _) -> if sub_op = oper
                                    then display(q)
                                    else "(" ^ display(q) ^ ")"
      | _ -> display(q))

exception EmptyBigConjOrDisj

let rec big_conj propositions =
  match propositions with
  | [] -> raise EmptyBigConjOrDisj
  | [p] -> p
  | p :: rest -> conj(p, big_conj rest)

let rec big_disj propositions =
  match propositions with
  | [] -> raise EmptyBigConjOrDisj
  | [p] -> p
  | p :: rest -> disj(p, big_disj rest)

(* Example *)

let orig = cond(big_conj([
                      cond(Var("t"), Var("u")); (* Careful, lists are delmited with ;*)
                      disj(Var("p"), Negative(Var("q")));
                      cond(Var("p"), cond(Var("u"), Var("r")));
                      Var("q")
                  ]), (* Tuples with ,*)
                cond(Var("t"), Var("r")))


let flip operation =
  match operation with
  | Conjunction -> Disjunction
  | Disjunction -> Conjunction


                 (* Warning is fine, read it you can see it is ok*)
let rec negation_normal_form argument =
  match argument with
  | Var(s) -> Var(s)
  | Negative(Var(s)) -> Negative(Var(s))
  | Negative(Negative(Var(p))) -> negation_normal_form(Var(p))
  | Negative(BinaryOper(oper, p, q)) ->
     BinaryOper(flip(oper),
                negation_normal_form(Negative(p)),
                negation_normal_form(Negative(q)))
  | BinaryOper(oper, p, q) -> BinaryOper(oper, negation_normal_form(p), negation_normal_form(q))
;;

(* Try this on the example*)
orig |> negation_normal_form |> display
;;

let rec distribute argument =
  match argument with
  | (p, BinaryOper(Conjunction, q, r)) ->
     BinaryOper(Conjunction, distribute(p, q), distribute(p, r))
  | (BinaryOper(Conjunction, p, q), r) ->
     BinaryOper(Conjunction, distribute(p, r), distribute(q, r))
  | (p, q) -> BinaryOper(Disjunction, p, q)

exception NotInNNF of string

let rec conjunctive_normal_form argument =
  match argument with
  | Var(s) -> Var(s)
  | Negative(Var(s)) -> Negative(Var(s))
  | Negative(p) -> raise (NotInNNF(display(p)))
  | BinaryOper(Conjunction, p, q) ->
     BinaryOper(Conjunction, conjunctive_normal_form(p), conjunctive_normal_form(q))
  | BinaryOper(Disjunction, p, q) ->
     distribute(conjunctive_normal_form p, conjunctive_normal_form q)

;;

(* example *)

orig |> negation_normal_form |> conjunctive_normal_form |> display

exception NotInCNF of string

let rec positives arguments =
  match arguments with
  | Var(s) -> [s]
  | Negative(Var(s)) -> []
  | BinaryOper(Disjunction, p, q) -> positives p @ positives q
  | x -> raise (NotInCNF(display(x)))

let rec negatives arguments =
  match arguments with
  | Var(s) -> []
  | Negative(Var(s)) -> []
  | BinaryOper(Disjunction, p, q) -> negatives p @ negatives q
  | x -> raise (NotInCNF(display(x)))
