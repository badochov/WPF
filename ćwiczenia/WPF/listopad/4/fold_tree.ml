(* drzewo binarne *)
type 'a tree = Node of 'a tree * 'a * 'a tree | Leaf

let rec fold_tree funkcja akumulator = function
  | Leaf ->
      akumulator
  | Node (lewe, wartosc, prawa) ->
      funkcja wartosc
        (fold_tree funkcja akumulator lewe)
        (fold_tree funkcja akumulator prawa)

(* czy drzewo to BST *)

(* 
    typ option wbudowany
    type 'a option = None | Some of 'a 
*)

exception NotBST

let default opt def = match opt with None -> Some def | Some _ -> opt

(* less than option *)
let lto a b = match (a, b) with Some av, Some bv -> av < bv | _ -> true

let f x (lmin, lmax) (rmin, rmax) =
  if lto lmax (Some x) && lto (Some x) rmin then
    (default lmin x, default rmax x)
  else raise NotBST

let is_bst t =
  try
    let _ = fold_tree f (None, None) t in
    true
  with NotBST -> false

(* lista w kolejnośći infixowej *)
(* bazowa funkcja rekurencyjna do przerobienia na folda *)
let rec bazowa_funkcja t ak =
  match t with
  | Leaf ->
      ak
  | Node (l, x, r) ->
      bazowa_funkcja l (x :: bazowa_funkcja r ak)

let rec funckja_do_folda t =
  match t with
  | Leaf ->
      fun ak -> ak
  | Node (l, x, r) ->
      fun ak -> bazowa_funkcja l (x :: bazowa_funkcja r ak)

(* 
    lf -> funkcja doklajająca drzewo lewe
    rf -> funckja doklejająca drzewo prawe
*)
let f wartosc lf rf l = lf (wartosc :: rf l)

(* tworzymy fooldem funkcje, która na liscie pustej jest wyprodukowana *)
let inorder t = fold_tree f (fun l -> l) t []

(* liczba elementów widocznych -> takich, które są najwyższe na ścieżce do siebie *)

(* werjsa rekurencyjna *)
let rec f t max_w =
  match t with
  | Leaf ->
      0
  | Node (l, w, r) ->
      if w > max_w then f l w + f r w + 1 else f l max_w + f r max_w

let widoczne_rek t = f t min_int

(* todo napisać to rekurencyjnie *)
(* też zwraca funckje *)
