type vegetable = Carrot | Zucchini | Tomato |
  Cucumber | Lettuce ;;
type grain = Wheat | Oat | Barley | Maize ;;
type tree = Oak | Elm | Maple | Spruce | Fir | Pine | Willow ;;

type plot = Grove of tree | Garden of vegetable |
Field of grain | Vacant ;;

let rotate plot = match plot with
    Vacant -> Field(Wheat)
  | Garden(Cucumber) -> Garden(Cucumber)
  | Garden(Lettuce) -> Field(Maize)
  | Garden(Tomato) -> Garden(Carrot)
  | Garden(_) -> Vacant
  | Field(Wheat) -> Garden(Tomato)
  | Field(Maize) -> Garden(Lettuce)
  | Field(_) -> Garden(Zucchini)
  | x -> x ;;