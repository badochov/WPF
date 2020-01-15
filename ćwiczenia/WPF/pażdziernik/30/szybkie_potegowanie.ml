(* łączenie funkcji *)
let compose f g x = f (g x)

(* funkcja identycznościowa *)
let id x = x

(* algo na szybkie potęgowanie *)
let pot ( * ) e n x =
  let rec pom n a w =
    if n = 0 then w
    else if n mod 2 = 1 then pom (n / 2) (a * a) (w * a)
    else pom (n / 2) (a * a) w
  in
  pom n x e

(* iteracja po liście, która ma rozmiar logariytmiczny ale jest liniowa liczba aplikacji i tak *)
(* drzewo parówa *)
let iter5 n f = pot compose id n f

let h5 = iter5 17 succ
