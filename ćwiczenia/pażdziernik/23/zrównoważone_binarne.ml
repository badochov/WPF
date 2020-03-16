(* zrównoważone drzewo binarne którego kolejność infixowa jest jak lista *)

type 'a tree =
  | Leaf
  | Node of 'a tree * 'a * 'a tree

let rec zrob n lista =
  if n = 0
  then (Leaf, lista)
  else
    let w_lewym = (n - 1) / 2 in
    let lewe, reszta1 = zrob w_lewym lista in
    let prawe, reszta2 = zrob (n - w_lewym - 1) (List.tl reszta1) in
    (Node (lewe, List.hd reszta1, prawe), reszta2)


let zbuduj l = zrob (List.length l) l
