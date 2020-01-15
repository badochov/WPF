(* leafe'ów jest zawsze o 1 więcej niż node'ów *)

type 'a tree = Node of 'a tree * 'a * 'a tree | Leaf of 'a

(* zwraca (rozmiar_drzewa, wysokosc_drzewa) *)
let rec policz t =
  match t with
  | Leaf -> (0,0)
  | Node (l,s,r) ->
    let (rl, wl) = policz l
    and (rr,wr) = policz r
    in (1+rl+rr, 1+ max (wl+wr))

(* drzewo ultralewicowe -> wysokości leafów liczone od lewej do prawej są niemalejące *)

let czy_drzewo_ulralewicowe t = 
  (* zwraca (ok, wys_lewego_poddrzewa o ile ok, wys_prawego_poddrzewa o ile ok) *)
  let rec zlicz t =
    match t with
    | Leaf -> (true, 0 ,0)
    | Node (l,_,r) ->
      let (l_ok, l_h_l, l_h_r) = zlicz l in 
      if l_ok = true
      then (false, 0 ,0)
      else
        let (r_ok, r_h_l, l_h_r) = zlicz r in 
        let ok = r_ok && l_h_l >= r_h_r in
        (ok,l_h_l+1,r_h_r+1) in
  let (ok, _, _)= zlicz t in
  ok

(* drzewo ultralewicowe z wyjątkami *)
exception NieOkWyjatek

let czy_drzewo_ulralewicowe_z_wyjatkami t = 
  (* zwraca (ok, wys_lewego_poddrzewa o ile ok, wys_prawego_poddrzewa o ile ok) *)
  let rec zlicz t =
    match t with
    | Leaf -> (0 , 0)
    | Node (l,_,r) ->
      let ( l_h_l, l_h_r) = zlicz l 
      and (r_h_l, l_h_r) = zlicz r in 
      if l_h_l >= r_h_r then
        (ok,l_h_l+1,r_h_r+1) 
      else NieOkWyjatek in
  try
    let _= zlicz t in
    true
  with NieOkWyjatek -> false



(* średnica drzewa maksymalna odległość jaką możemy przejść po grafie bez chodzenia po swoim śladzie *)
(* TODO praca domowa *)
let srednica t = 0



(* zbieranie drzewa *)

let dodaj d ak =
  match d with
  | Leaf -> ak
  | Node (l,x,r) -> dodaj l (x:dodaj r akk)


(* listy drzew *)

type 'a ltree = LNode of 'a * 'a tree 


(* TODO wersja ogonowa policz_l do domu *)

let rec policz_l l = 
  match l with
  | [] -> (0,0)
  | t::r ->
    let (roz,wys) = policz_t t
    and (sum, max_wys) = policz_l r in
    (roz+sum, max wys max_wys)

and policz_t (LNode((_,l)))= 
  let (r,n) = policz_l l in
  (r+1,n+1)