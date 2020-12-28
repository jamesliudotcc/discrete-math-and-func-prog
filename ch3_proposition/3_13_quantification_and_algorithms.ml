(* this is the funciton on 135 translated to OCaml *)

let rec all_have_divisor (l1, l2) =
  match (l1, l2) with
  | ([], yy) -> true
  | (x::x_rest, yy) ->
      let rec has_divisor yy =
        match yy with
        | [] -> false
        | y::y_rest ->
            if x mod y = 0 then true
            else has_divisor y_rest
      in
        has_divisor yy && all_have_divisor (x_rest, yy)


let rec all_less_than (l1, l2) =
  match (l1, l2) with
  | ([], yy) -> true
  | (x::x_rest, yy) ->
      let rec less_than yy =
        match yy with
        | [] -> false
        | y::y_rest ->
            if x < y then true
            else less_than y_rest
      in
        less_than yy && all_less_than (x_rest, yy)

let rec all_have_double (l1, l2) =
  match (l1, l2) with
  | ([], yy) -> true
  | (x::x_rest, yy) ->
     let rec have_double yy =
       match yy with
       | [] -> false
       | y::y_rest ->
          if y / 2 = x && y mod 2 = 0
          then true
          else have_double y_rest
     in
       have_double yy && all_have_double (x_rest, yy)

(* This is wrong. Fix. *)
let rec all_have_inv l =
  match l with
  | [] -> true
  | head::rest ->
    let rec remaining_have_inv l1 =
      let rec have_inv l2 =
        match l with
        | [] -> false
        | l2_head::l2_rest ->
            if head + l2_head = 0
            then true
            else have_inv l2_rest
       in
       have_inv rest
    in
    remaining_have_inv rest


let rec has_divisor_of_all l =
  match l with
  | [] -> true
  | x::x_rest ->
     let rec have_divisor l1 =
       match l1 with
       | [] -> false
       | xx::xx_rest ->
          if (x mod xx = 0 || xx mod x = 0) then true
          else have_divisor xx_rest
     in
       have_divisor l
