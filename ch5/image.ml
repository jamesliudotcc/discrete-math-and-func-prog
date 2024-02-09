let image (element, relation) =
  let rec process_relation pairs =
    match pairs with
    | [] -> []
    | (domain, range)::tail -> if domain = element
                               then range::process_relation tail
                               else process_relation tail
  in process_relation relation

let add_image (elem, set) =
    let rec process_set range =
      match range with
      | [] -> []
      | head::tail -> (elem, head)::process_set(tail)
    in process_set set

let composition (r, s) =
  let rec do_composition pairs =
    match pairs with
    | [] -> []
    | (a, b)::tail -> add_image (a, image (b, s)) @ do_composition tail
  in do_composition r
