type tree = Oak | Elm | Maple | Spruce | Fir | Pine | Willow ;;
type treeDivision = Broadleaf | Coniferous ;;

let growthRate(x) = match x with
  Elm -> 1.3
  | Maple -> 2.7
  | Oak -> 2.4
  | Pine -> 0.4
  | Spruce -> 2.9
  | Fir -> 1.1
  | Willow -> 5.3 ;;
  
let growingMonths(division) = match division with 
  Broadleaf -> 6.0
  | Coniferous -> 12.0 ;;
  
let division genus = match genus with 
  Pine -> Coniferous
  | Spruce -> Coniferous
  | Fir -> Coniferous
  | _ -> Broadleaf ;;
  
let predictHeight (genus, initial, years) =
initial +. (years *. growingMonths(division(genus)) *. growthRate(genus));;
