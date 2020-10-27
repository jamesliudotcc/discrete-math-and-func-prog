let f_log x base =
  let rec find_f_log f_log remainder =
    if remainder < (exp_fast base f_log)
    then f_log, remainder
    else 5, 5 (* Obviously this is wrong, fix me*)
  in find_f_log 0 base
