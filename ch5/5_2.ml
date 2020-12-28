type city = Vancouver | LosAngeles | Mexico | Minneapolis |
            Omaha | KansasCity | Denver | StLouis |
            Memphis | Chicago | NewOrleands | Cincinnati |
            Pittsburgh | Montreal | NewYork

type river = Missouri | Platte | NPlatte | SPlatte |
             Arkansas | Canadian | Kansas | Mississippi |
             Tennessee | Ohio | Allegheny | Monogahela

let is_on_river = [(Denver, Platte); (Omaha, Missouri); (Omaha, Platte);
                   (KansasCity, Missouri); (KansasCity, Kansas);
                   (Minneapolis,Mississippi); (StLouis, Mississippi);
                   (StLouis, Missouri); (Memphis, Mississippi);
                   (NewOrleands, Mississippi); (Cincinnati, Ohio);
                   (Pittsburgh, Ohio); (Pittsburgh, Allegheny);
                   (Pittsburgh, Monogahela);]

let flows_into = [(Platte, Missouri); (Kansas, Missouri);
                  (Missouri, Mississippi); (Canadian, Arkansas);
                  (Arkansas, Mississippi); (Allegheny, Ohio);
                  (Monogahela, Ohio); (Tennessee, Ohio);
                  (NPlatte, Platte); (SPlatte, Platte);
                  (Ohio, Mississippi);]

let rec is_related_to a b l =
  match a, b, l with
  | a, b, [] -> false
  | a, b, (h1, h2)::rest ->
     (a = h1 && b = h2) || is_related_to a b rest
