(** Autor: Hubert Badocha *)

(** Review: Szymon Michniak *)

(** Punkt na płaszczyźnie *)
type point = float * float

(** Poskładana kartka: ile razy kartkę przebije szpilka wbita w danym punkcie *)
type kartka = point -> int

(** interpretacja prostej *)
type prosta = {
  a : float;
  b : float;
  c : float;
}

(** [prostokat p1 p2] zwraca kartkę, reprezentującą domknięty prostokąt o
    bokach równoległych do osi układu współrzędnych i lewym dolnym rogu
    [p1] a prawym górnym [p2]. Punkt [p1] musi więc być nieostro na lewo i w
    dół od punktu [p2]. Gdy w kartkę tę wbije się szpilkę wewnątrz (lub
    na krawę1dziach) prostokąta, kartka zostanie przebita 1 raz, w
    pozostałych przypadkach 0 razy *)
let prostokat (ld_x, ld_y) (pg_x, pg_y) (p_x, p_y) =
  if (p_x < ld_x || p_x > pg_x) || p_y < ld_y || p_y > pg_y then 0 else 1

(** funkcja licząca odległość między dwoma punktami *)
let odleglosc (x1, y1) (x2, y2) =
  sqrt (((x2 -. x1) ** 2.) +. ((y2 -. y1) ** 2.))

(** [kolko p r] zwraca kartkę, reprezentującą kółko domknięte o środku w
    punkcie [p] i promieniu [r] *)
let kolko srodek promien pkt = if odleglosc srodek pkt > promien then 0 else 1

(** tworzy prostą przechodzącą przez 2 punkty *)
let stworz_prosta (x1, y1) (x2, y2) =
  let a = y1 -. y2
  and b = x2 -. x1 in
  let c = -.(y1 *. b) -. (x1 *. a) in
    {a; b; c}

(** sprawdza po której stronie prostej leży punkt 
    Zwraca liczbe zmiennoprzecinkową
    0. -> na prostej 
    > 0. -> prawej
    < 0. -> po lewej *)
let strona_prostej prosta (x, y) =
  (prosta.a *. x) +. (prosta.b *. y) +. prosta.c

(** Liczy mianownik do wzoru na odbicie prostej *)
let mianownik a b = (a ** 2.) +. (b ** 2.)

(** Przyjmuje prostą i punkt zwraca współrzędną x po odbiciu punktu przez
    prostą *)
let odbij_x {a; b; c} (x, y) =
  let l =
    (((b ** 2.) -. (a ** 2.)) *. x) -. (2. *. a *. b *. y) -. (2. *. a *. c)
  in
    l /. mianownik a b

(** Przyjmuje prostą i punkt zwraca współrzędną y po odbiciu punktu przez
    prostą *)
let odbij_y {a; b; c} (x, y) =
  let l =
    (((a ** 2.) -. (b ** 2.)) *. y) -. (2. *. a *. b *. x) -. (2. *. b *. c)
  in
    l /. mianownik a b

(** zwraca co sie dzieje z punktem po złożeniu przyjmuje proostą i punkt
    zwraca liste punktów *)
let punkty_po_zlozeniu prosta p =
  let strona = strona_prostej prosta p in
    if strona = 0.
    then [p]
    else if strona < 0.
    then []
    else
      let x = odbij_x prosta p
      and y = odbij_y prosta p in
        [p; (x, y)]

(** [zloz p1 p2 k] składa kartkę [k] wzdłuż prostej przechodzącej przez
    punkty [p1] i [p2] (muszą to być różne punkty). Papier jest składany w
    ten sposób, że z prawej strony prostej (patrząc w kierunku od [p1] do
    [p2]) jest przekładany na lewą. Wynikiem funkcji jest złożona kartka.
    Jej przebicie po prawej stronie prostej powinno więc zwrócić 0.
    Przebicie dokładnie na prostej powinno zwrócić tyle samo, co przebicie
    kartki przed złożeniem. Po stronie lewej - tyle co przed złożeniem plus
    przebicie rozłożonej kartki w punkcie, który nałożył się na punkt
    przebicia. *)
let zloz p1 p2 k pkt =
  let zlicz ak punkt = ak + k punkt in
  let prosta = stworz_prosta p1 p2 in
  let punkty = punkty_po_zlozeniu prosta pkt in
    List.fold_left zlicz 0 punkty

(** [skladaj \[(p1_1,p2_1);...;(p1_n,p2_n)\] k = zloz p1_n p2_n (zloz ... (zloz
    p1_1 p2_1 k)...)] czyli wynikiem jest złożenie kartki [k] kolejno
    wzdłuż wszystkich prostych z listy *)
let skladaj proste k =
  List.fold_left (fun ak (p1, p2) -> zloz p1 p2 ak) k proste

(* Testy *)
(* let test a b msg = if a <> b then ( print_int a ; print_string "<>" ;
   print_int b ; print_string " test: " ; print_endline msg )

   let p1 = prostokat (0., 0.) (10., 10.)

   let k1 = kolko (5., 5.) 5.

   let l1 = [ ((0., 0.), (10., 10.)); ((5., 0.), (10., 5.)); ((10., 0.), (0.,
   10.)); ((2.5, 0.), (2.5, 10.)); ]

   let l2 = [ ((8., 0.), (10., 2.)); ((6., 0.), (10., 4.)); ((4., 0.), (10.,
   6.)); ((2., 0.), (10., 8.)); ((0., 0.), (10., 10.)); ((0., 2.), (8., 10.));
   ((0., 4.), (6., 10.)); ((0., 6.), (4., 10.)); ((0., 8.), (2., 10.)); ]

   let p2 = skladaj l1 p1

   let p3 = skladaj l2 p1

   let k2 = skladaj l1 k1

   ;; test (p2 (7., 3.)) 0 "0.1: p2"

   ;; test (p2 (5., 8.)) 0 "0.2: p2"

   ;; test (p2 (3., 5.)) 0 "0.3: p2"

   ;; test (p2 (5., 5.)) 0 "0.4: p2"

   ;; test (p2 (0., 0.)) 2 "1: p2"

   ;; test (p2 (0., 10.)) 2 "2: p2"

   ;; test (p2 (2.5, 2.5)) 2 "3: p2"

   ;; test (p2 (2.5, 7.5)) 2 "4: p2"

   ;; test (p2 (2.5, 5.)) 4 "5: p2"

   ;; test (p2 (0., 5.)) 5 "6: p2"

   ;; test (p2 (1., 2.)) 4 "7: p2"

   ;; test (p2 (1., 5.)) 8 "8: p2"

   ;; test (p2 (1., 8.)) 4 "9: p2"

   ;; test (k2 (7., 3.)) 0 "0.1: k2"

   ;; test (k2 (5., 8.)) 0 "0.2: k2"

   ;; test (k2 (3., 5.)) 0 "0.3: k2"

   ;; test (k2 (5., 5.)) 0 "0.4: k2"

   ;; test (k2 (2.5, 2.5)) 2 "1: k2"

   ;; test (k2 (2.5, 7.5)) 2 "2: k2"

   ;; test (k2 (2.5, 5.)) 4 "3: k2"

   ;; test (k2 (0., 5.)) 5 "4: k2"

   ;; test (k2 (1., 3.)) 4 "5: k2"

   ;; test (k2 (1., 5.)) 8 "6: k2"

   ;; test (k2 (1., 7.)) 4 "7: k2"

   ;; test (p3 (-4., 6.)) 2 "1: p3"

   ;; test (p3 (-3., 5.)) 1 "2: p3"

   ;; test (p3 (-3., 7.)) 2 "3: p3"

   ;; test (p3 (-2., 6.)) 3 "4: p3"

   ;; test (p3 (-2.5, 6.5)) 4 "5: p3"

   ;; test (p3 (-2., 8.)) 4 "6: p3"

   ;; test (p3 (-1., 7.)) 3 "7: p3"

   ;; test (p3 (-1.5, 7.5)) 6 "8: p3"

   ;; test (p3 (0., 8.)) 5 "9: p3"

   ;; test (p3 (-1., 9.)) 4 "10: p3"

   ;; test (p3 (-0.5, 8.5)) 8 "11: p3"

   ;; test (p3 (0., 10.)) 6 "12: p3"

   ;; test (p3 (1., 9.)) 5 "13: p3"

   ;; test (p3 (0.5, 9.5)) 10 "14: p3"

   ;; let kolo = kolko (0., 0.) 10. in assert (kolo (1000., 0.) = 0) ; let
   poziomo = zloz (0., 0.) (1., 0.) kolo in assert (poziomo (0., 0.) = 1) ;
   assert (poziomo (0., 1.) = 2) ; assert (poziomo (0., -1.) = 0) ; let pionowo
   = zloz (0., 0.) (0., 1.) kolo in assert (pionowo (0., 0.) = 1) ; assert
   (pionowo (-1., 0.) = 2) ; assert (pionowo (1., 0.) = 0) ; let cwiartka =
   zloz (0., 0.) (0., 1.) poziomo in assert (cwiartka (0., 0.) = 1) ; assert
   (cwiartka (-1., 1.) = 4) ; assert (cwiartka (-1., 0.) = 2) *)
