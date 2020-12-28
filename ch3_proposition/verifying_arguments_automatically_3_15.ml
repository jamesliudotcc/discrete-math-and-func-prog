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
