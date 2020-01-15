type przedzial = {min: float; max: float}
(** typ reprezentujący przedzial od min do max włącznie *)

type wartosc = przedzial list
(** typ reprezentujacy wartość w formie posortowanej,
    według minimalnej wartośći w przedziale,
    listy przedziałów *)

(** mnożenie floatów które dla nieskończonośc * 0 daje 0  *)
let ( *.. ) (x : float) (y : float) =
  let czy_inf liczba = classify_float liczba = FP_infinite in
  if czy_inf x && y = 0. then 0.
  else if czy_inf y && x = 0. then 0.
  else x *. y

(** daje poprawny znak zeru na krancu wartości *)
let oznakuj_zero wart =
  (* daje poprawny znak zeru na krańcu przedziału *)
  let oznakuj_przedzial p =
    let p_pom = if p.min = 0. then {min= 0.; max= p.max} else p in
    if p.max = 0. then {min= p_pom.min; max= -0.} else p_pom
  in
  List.map oznakuj_przedzial wart

(* konstruktory *)

(** tworzy przedział od do podanej wartości *)
let przedzial_od_do liczba_min liczba_max = {min= liczba_min; max= liczba_max}

(** zwraca wartość, której minimum i maximum sa podane *)
let wartosc_od_do liczba_min liczba_max =
  oznakuj_zero [przedzial_od_do liczba_min liczba_max]

(** zwraca przedzial reprezentujący liczbę o danej dokładności
  liczba dokladnosc = liczba +/- dokladnosc%  *)
let wartosc_dokladnosc liczba dokladnosc =
  (* sprawdza czy liczba to nan *)
  let procent_liczby = dokladnosc /. 100. *. liczba in
  let sum = liczba +. procent_liczby and diff = liczba -. procent_liczby in
  wartosc_od_do (min sum diff) (max sum diff)

(** wartość której średnia to dana liczba o idealnej dokładności  *)
let wartosc_dokladna liczba = wartosc_od_do liczba liczba

(** zwraca wartość która jest zbiorem wszystkich liczb rzeczywistych *)
let wartosc_rzecz = wartosc_od_do neg_infinity infinity

(* selektory *)

(** sprawdza czy liczba jest w wartości *)
let rec in_wartosc wart liczba =
  match wart with
  | [] ->
      false
  | przedzial :: reszta ->
      if liczba >= przedzial.min && liczba <= przedzial.max then true
      else in_wartosc reszta liczba

(** sprawdza czy wartosć to nan *)
let czy_nan wart = match wart with [] -> true | _ -> false

(** zwraca najmniejszą wartość liczbową w wartości *)
let min_wartosc wart = if czy_nan wart then nan else (List.hd wart).min

(** zwraca największą wartość liczbowa w wartości *)
let max_wartosc wart =
  if czy_nan wart then nan else (List.hd (List.rev wart)).max

(** zwraca średnią wartość wartośći określoną jako
   (min_wartość_w_wartosci + max_wartośc_w_wartosci)/2 *)
let sr_wartosc wart =
  let min_w, max_w = (min_wartosc wart, max_wartosc wart) in
  (min_w +. max_w) /. 2.

(** złącza o ile można pierwszy przedział wartosci i dany przedział *)
let zlacz_przedzialy wart przed =
  match wart with
  | [] ->
      [przed]
  | p :: reszta as w ->
      if p.max >= przed.min then
        {min= p.min; max= max p.max przed.max} :: reszta
      else przed :: w

(** złącza przedziały w wartości i sortuje je *)
let zlacz_przedzialy_w_wartosci wart =
  (* funkcja porównawcza do sortowania wartości *)
  let porownaj_wartosc w1 w2 =
    if w1.min > w2.min then 1 else if w1.min = w2.min then 0 else -1
  in
  let posortowana_wartosc = List.sort porownaj_wartosc wart in
  let zlaczona_wartosc =
    List.fold_left zlacz_przedzialy [] posortowana_wartosc
  in
  List.rev (oznakuj_zero zlaczona_wartosc)

(** wykonuje podaną akcję przyjmującą i zwracającą floaty na przedziałach,
   tworzy na jej podstawie przedział
   od najmniejszej nie-nanowej wartości do najwiekszej
   i dokleja go na początek akumulatora *)
let wykonaj_funkcje_na_przedzialach funkcja p2 ak p1 =
  let pom =
    [ funkcja p1.min p2.min
    ; funkcja p1.min p2.max
    ; funkcja p1.max p2.min
    ; funkcja p1.max p2.max ]
  in
  let filtered_pom = List.filter (fun w1 -> classify_float w1 <> FP_nan) pom in
  let pom_min = List.fold_left min infinity filtered_pom
  and pom_max = List.fold_left max neg_infinity filtered_pom in
  przedzial_od_do pom_min pom_max :: ak

(** opakowuję podawną funkcję w format przyjazny procedurze operacja *)
let funkcja_op funkcja wart ak przed =
  List.fold_left (wykonaj_funkcje_na_przedzialach funkcja przed) [] wart @ ak

(** wykonuję podana 3 argumentową funkcję
    na wszystkich parach przedziałów z wart1 wart2
    format funkcji podany niżej *)
let operacja (funkcja : wartosc -> wartosc -> przedzial -> wartosc) wart1 wart2
    =
  let wartosc_po_operacji = List.fold_left (funkcja wart1) [] wart2 in
  zlacz_przedzialy_w_wartosci wartosc_po_operacji

(** wykonuje dodawanie wartości *)
let plus wart1 wart2 = operacja (funkcja_op ( +. )) wart1 wart2

(** wykonuje odejmowanie wartości *)
let minus wart1 wart2 = operacja (funkcja_op ( -. )) wart1 wart2

(** wykonuje mnożenie wartości *)
let razy wart1 wart2 = operacja (funkcja_op ( *.. )) wart1 wart2

(** wykonuje dzielenie wartości *)

(** rozłacza przedział na dwa od zera i do zera
    jeżeli w występuje w nim zero dodatnie i ujemne *)
let rozlacz_przedzialy_zerem wart przed =
  if przed.min < 0. && przed.max > 0. then
    przedzial_od_do przed.min (-0.) :: przedzial_od_do 0. przed.max :: wart
  else przed :: wart

let podzielic wart1 wart2 =
  if wart2 = wartosc_od_do 0. 0. then []
  else
    let rozlaczona_wart2 = List.fold_left rozlacz_przedzialy_zerem [] wart2 in
    operacja (funkcja_op ( /. )) wart1 rozlaczona_wart2

(* 
let is_nan x = compare x nan = 0;;

let a = wartosc_od_do (-1.) 1.            (* <-1, 1> *)
let b = wartosc_dokladna (-1.)            (* <-1, -1> *)
let c = podzielic b a                     (* (-inf -1> U <1 inf) *)
let d = plus c a                          (* (-inf, inf) *)
let e = wartosc_dokladna 0.               (* <0, 0> *)
let f = razy c e                          (* <0, 0> *)
let g = razy d e                          (* <0, 0> *)
let h = wartosc_dokladnosc (-10.) 50.     (* <-15, -5> *)
let i = podzielic h e                     (* nan, przedzial pusty*)
let j = wartosc_od_do (-6.) 5.            (* <-6, 5> *)
let k = razy j j                          (* <-30, 36> *)
let l = plus a b                          (* <-2, 0> *)
let m = razy b l                          (* <0, 2> *)
let n = podzielic l l                     (* <0, inf) *)
let o = podzielic l m                     (* (-inf, 0) *)
let p = razy o a                          (* (-inf, inf) *)
let q = plus n o                          (* (-inf, inf) *)
let r = minus n n                         (* (-inf, inf) *)
let s = wartosc_dokladnosc (-0.0001) 100. (* <-0.0002, 0> *)
let t = razy n s;;                        (* (-inf, 0) *)

assert ((min_wartosc c, max_wartosc c) = (neg_infinity, infinity));
assert (is_nan (sr_wartosc c) );
assert (not (in_wartosc c 0.));
assert ((in_wartosc c (-1.)) && (in_wartosc c (-100000.)) && (in_wartosc c 1.) && (in_wartosc c 100000.));
assert ((in_wartosc d 0.) && (in_wartosc d (-1.)) && (in_wartosc d (-100000.)) && (in_wartosc d 1.) && (in_wartosc d 100000.));
assert ((min_wartosc f, max_wartosc f, sr_wartosc f) = (0., 0., 0.));
assert ((min_wartosc g, max_wartosc g, sr_wartosc g) = (0., 0., 0.));
assert ((min_wartosc h, max_wartosc h, sr_wartosc h) = (-15., -5., -10.));
assert (is_nan (min_wartosc i) && is_nan (sr_wartosc i) && is_nan (max_wartosc i));
assert ((min_wartosc k, max_wartosc k, sr_wartosc k) = (-30., 36., 3.));
assert ((min_wartosc n, max_wartosc n, sr_wartosc n) = (0., infinity, infinity));
assert ((min_wartosc o, max_wartosc o, sr_wartosc o) = (neg_infinity, 0., neg_infinity));
assert ((min_wartosc p, max_wartosc p, is_nan (sr_wartosc p)) = (neg_infinity, infinity, true));
assert ((min_wartosc q, max_wartosc q, is_nan (sr_wartosc q)) = (neg_infinity, infinity, true));
assert ((min_wartosc r, max_wartosc r, is_nan (sr_wartosc r)) = (neg_infinity, infinity, true));
assert ((min_wartosc t, max_wartosc t, sr_wartosc t) = (neg_infinity, 0., neg_infinity));;

let a = wartosc_od_do neg_infinity infinity
let c = plus a a
let d = razy a a
let e = podzielic a a
let f = minus a a;;
assert ((min_wartosc c, max_wartosc c, is_nan (sr_wartosc c)) = (neg_infinity, infinity, true));
assert ((min_wartosc d, max_wartosc d, is_nan (sr_wartosc d)) = (neg_infinity, infinity, true));
assert ((min_wartosc e, max_wartosc e, is_nan (sr_wartosc e)) = (neg_infinity, infinity, true));
assert ((min_wartosc d, max_wartosc d, is_nan (sr_wartosc d)) = (neg_infinity, infinity, true));;

let a = wartosc_od_do 0. infinity
let b = wartosc_dokladna 0.
let c = podzielic a b
let d = podzielic b b;;
assert ((is_nan(min_wartosc c), is_nan(max_wartosc c), is_nan (sr_wartosc c)) = (true, true, true));
assert ((is_nan(min_wartosc d), is_nan(max_wartosc d), is_nan (sr_wartosc d)) = (true, true, true));;

let a = wartosc_od_do (-10.) 10.
let b = wartosc_od_do (-1.) 1000.
let c = podzielic a b;;
assert ((min_wartosc c, max_wartosc c, is_nan (sr_wartosc c)) = (neg_infinity, infinity, true));;

let a = wartosc_od_do (-1.0) 1.0
let b = wartosc_dokladna 1.0
let c = podzielic b a
let d = wartosc_dokladna 3.0
let e = plus c d      (* (-inf, 2> U <4 inf) *)
let f = podzielic b e (* (-inf, 1/4> U <1/2, inf) *)
let g = podzielic d a (* (-inf, -3> U <3, inf) *)
let h = podzielic g f (* (-inf, inf *)
let i = plus f g;;    (* (-inf, inf) *)

assert ((in_wartosc f 0.25, in_wartosc f 0.26, in_wartosc f 0.49, in_wartosc f 0.50)=(true, false, false, true));
assert ((min_wartosc h, max_wartosc h, is_nan (sr_wartosc h), in_wartosc h 0.) = (neg_infinity, infinity, true, true));
assert ((min_wartosc h, max_wartosc h, is_nan (sr_wartosc h), in_wartosc h 0.3) = (neg_infinity, infinity, true, true));;

let jed = wartosc_dokladna 1.
let zero = wartosc_dokladna 0.;;
assert ((sr_wartosc zero, max_wartosc zero, min_wartosc zero) = (0.,0.,0.));;

let a = wartosc_od_do 0. 1. (* <0,1> *)
let b = podzielic a a       (* <0, inf)*)
let c = razy b zero;;       (* <0,0> *)
assert ((sr_wartosc c, max_wartosc c, min_wartosc c) = (0.,0.,0.));;

let a = podzielic jed zero;; (* nan *)
assert (is_nan (min_wartosc a));
assert (is_nan (max_wartosc a));
assert (is_nan (sr_wartosc a));;

let a = wartosc_dokladnosc 1. 110.;; (* <-0.1, 2.1> *)
assert (in_wartosc a (-.0.1));
assert (in_wartosc a (2.1));;

let a = wartosc_od_do (-.3.) 0.  (* <-3.0, 0.0> *)
let b = wartosc_od_do 0. 1.      (* <-0.0, 1.0> *)
let c = podzielic a b;;          (* (-inf, 0> *)
assert (max_wartosc c = 0.);
assert (min_wartosc c = neg_infinity);
assert (sr_wartosc c = neg_infinity);;

let a = wartosc_od_do 1. 4.     (* <1.0, 4.0> *)
let b = wartosc_od_do (-.2.) 3. (* <-2.0, 3.0> *)
let c = podzielic a b           (* (-inf, -1/2> U <1/3, inf) *)
let d = podzielic c b           (* (-inf, -1/6> U <1/9, inf) *)
let e = plus d jed              (* (-inf, 5/6> U <10/9, inf) *)
let f = sr_wartosc (podzielic jed (wartosc_dokladna 9.));; (* 1/9 *)
assert (is_nan (sr_wartosc d));
assert (in_wartosc d 0.12);
assert (not (in_wartosc d 0.));
assert (not (in_wartosc d (-0.125)));
assert (in_wartosc d f);
assert (not (in_wartosc e 1.));;

(* uwaga, ten test moze sie zawiesic przy pewnych implementacjach! *)
let a = wartosc_od_do (-2.) 3.
let b = wartosc_od_do 2. 3.
let c = podzielic b a

let rec iteruj f n acc = match n with
    | 0 -> acc
    | n when n > 0 -> iteruj f (n-1) (f acc acc)
    | _ -> acc

let x = iteruj razy 10 c;;
assert (not (in_wartosc x 0.));;
 *)
