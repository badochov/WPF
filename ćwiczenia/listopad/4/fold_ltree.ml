(* drzewo z listą synów *)
type 'a ltree = LNode of 'a * 'a ltree list

let rec fold_ltree f (LNode (x, l)) = f x (List.map (fold_ltree f) l)

(* zliczanie ile elemntów jest w drzewie *)

let flicz _ l = List.fold_left ( + ) 1 l

let licz t = fold_ltree flicz t

exception NotEqualHeight

(* sprawdzanie czy wszędzie są takie same wysokości *)

let wysokosc_dzieci _ l =
  match l with
  | [] ->
      0
  | h :: t ->
      if List.for_all (( = ) h) t then h else raise NotEqualHeight

let rowna_wysokosc t =
  try
    let _ = fold_ltree wysokosc_dzieci t in
    true
  with NotEqualHeight -> false
