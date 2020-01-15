(* Autor: Hubert Badocha *)
(* Recenzent Tomasz Nitsch *)

(* typy *)

(** typ pojedyńczego przedziału *)

type przedzial =
  { min : float
  ; max : float
  }

(** typ wartości mogącego być sumą 2 przedziałów lub przedziałem *)
type wartosc =
  | SumaPrzedzialow of przedzial * przedzial
  | Przedzial of przedzial

(* helpers *)

(** sprawdza czy liczba to nan *)
let czy_nan liczba = classify_float liczba = FP_nan

(* mnożenie floatów które dla nieskończonośc * 0 daje 0  *)
let ( *.. ) (x : float) (y : float) =
  let czy_inf liczba = classify_float liczba = FP_infinite in
  if czy_inf x && y = 0.
  then 0.
  else if czy_inf y && x = 0.
  then 0.
  else x *. y


(** zwraca przeciwność wartości -> -1 razy wartość *)
let przeciwnosc wart =
  (* zwraca przeciwność przedziału -> -1 razy przedział *)
  let przeciwnosc_przedzialu p = { min = -1. *. p.max; max = -1. *. p.min } in
  match wart with
  | Przedzial p ->
      Przedzial (przeciwnosc_przedzialu p)
  | SumaPrzedzialow (p1, p2) ->
      SumaPrzedzialow (przeciwnosc_przedzialu p2, przeciwnosc_przedzialu p1)


(** daje poprawny znak zeru na krancu wartości *)
let oznakuj_zero wart =
  (* daje poprawny znak zeru na krańcu przedziału *)
  let oznakuj_przedzial p =
    let p_pom = if p.min = 0. then { min = 0.; max = p.max } else p in
    if p.max = 0. then { min = p_pom.min; max = -0. } else p_pom
  in
  match wart with
  | Przedzial p ->
      Przedzial (oznakuj_przedzial p)
  | SumaPrzedzialow (p1, p2) ->
      SumaPrzedzialow (oznakuj_przedzial p1, oznakuj_przedzial p2)


(* konstruktory *)

(** zwraca wartość, której minimum i maximum sa podane *)
let wartosc_od_do liczba_min liczba_max =
  oznakuj_zero (Przedzial { min = liczba_min; max = liczba_max })


(** zwraca wartość reprezentującą nan *)
let wartosc_nan = wartosc_od_do nan nan

(** zwraca przedzial reprezentujący liczbę o danej dokładności *)
let wartosc_dokladnosc liczba dokladnosc =
  let procent_liczby = dokladnosc /. 100. *. liczba in
  let sum = liczba +. procent_liczby
  and diff = liczba -. procent_liczby in
  if czy_nan sum || czy_nan diff
  then wartosc_nan
  else wartosc_od_do (min sum diff) (max sum diff)


(** wartość której średnia to dana liczba o idealnej dokładności  *)
let wartosc_dokladna liczba = wartosc_od_do liczba liczba

(** zwraca wartość która jest zbiorem wszystkich liczb rzeczywistych *)
let wartosc_rzecz = wartosc_od_do neg_infinity infinity

(** konstruuje parę przedziałów, gdzie:
  pierwszy przedzial zaczyna się od neg_inifinity i kończy na liczba_max
  a drugi zaczyna od liczba_min i konczy na liczba max
  lub przedzial jeżeli liczba_max >= liczba_min,
  jako wartość *)
let wartosc_do_od liczba_max liczba_min =
  if liczba_max < liczba_min
  then
    SumaPrzedzialow
      ( { min = neg_infinity; max = liczba_max }
      , { min = liczba_min; max = infinity } )
  else wartosc_rzecz


(* selektory *)

(** sprawdza czy liczba jest w wartości *)
let in_wartosc wart liczba =
  match wart with
  | Przedzial przed ->
      przed.min <= liczba && liczba <= przed.max
  | SumaPrzedzialow (zak1, zak2) ->
      zak1.max >= liczba || zak2.min <= liczba


(** zwraca najmniejszą wartość liczbową w wartości *)
let min_wartosc wart =
  match wart with
  | Przedzial przed ->
      przed.min
  | SumaPrzedzialow (_, _) ->
      neg_infinity


(** zwraca największą wartość liczbowa w wartości *)
let max_wartosc wart =
  match wart with
  | Przedzial przed ->
      przed.max
  | SumaPrzedzialow (_, _) ->
      infinity


(** sprawdza czy wartosć to nan *)
let czy_wartosc_nan wart =
  match wart with
  | Przedzial p ->
      classify_float p.min = FP_nan
  | SumaPrzedzialow _ ->
      false


(** zwraca średnią wartość wartośći określoną jako
   (min_wartość_w_wartosci + max_wartośc_w_wartosci)/2 *)
let sr_wartosc wart =
  if czy_wartosc_nan wart
  then nan
  else
    let min_w, max_w = (min_wartosc wart, max_wartosc wart) in
    (min_w +. max_w) /. 2.


(** sprawdza, czy +0. jest w wartości *)
let dodatnie_zero_w_wartosci wart =
  match wart with
  | Przedzial p ->
      p.max > 0. && p.min <= 0.
  | SumaPrzedzialow (p1, p2) ->
      p1.max > 0. || p2.min < 0.


(** sprawdza, czy -0. jest w przedziale *)
let ujemne_zero_w_wartosci wart = dodatnie_zero_w_wartosci (przeciwnosc wart)

(* helpers v2 *)

(** operacja oczyszczająca wykonywana przy każdym działaniu
  sprawdza czy, któraś z wartośći to nan i oznacza zero po wykonaniu akcji,
  przyjmuje 2 wartości i fukcję dwuargumentową  zwracającą wartość
   *)

let oczysc wartosc1 wartosc2 funkcja =
  if czy_wartosc_nan wartosc1 || czy_wartosc_nan wartosc2
  then wartosc_nan
  else
    let pom = funkcja wartosc1 wartosc2 in
    oznakuj_zero pom


(** sprawdza czy podana liczba należy do zbioru liczb rzeczywistych *)
let rzeczywista num =
  match classify_float num with
  | FP_normal | FP_subnormal | FP_zero ->
      true
  | FP_infinite | FP_nan ->
      false


(** jeżeli przedziały w sumie przedziałów mogą być złaczone łaczy je 
  przedzial1.min <= przedzial2.min i zwraca wartość *)
let zlacz_sume_przedzialow suma_przedzialow =
  let przedzial1, przedzial2 = suma_przedzialow in
  if przedzial1.max >= przedzial2.min
  then wartosc_od_do przedzial1.min (max przedzial2.max przedzial1.max)
  else SumaPrzedzialow (przedzial1, przedzial2)


(* modyfikatory *)

(** złącza ze sobą 2 wartości *)
let zlacz_wartosci wart1 wart2 =
  if czy_wartosc_nan wart1 || czy_wartosc_nan wart2
  then wartosc_nan
  else
    (* tak jak funkcja matka ale nie sprawdza nan przy każdej iteracje*)
    let rec zlacz_wartosci_pom w1 w2 =
      match (w1, w2) with
      | Przedzial p1, Przedzial p2 ->
          zlacz_sume_przedzialow (p1, p2)
      | Przedzial przed, SumaPrzedzialow (p1, p2)
      | SumaPrzedzialow (p1, p2), Przedzial przed ->
          if przed.min <= p1.max
          then
            zlacz_wartosci_pom
              (zlacz_sume_przedzialow (p1, przed))
              (Przedzial p2)
          else if przed.min <= p2.min
          then
            zlacz_wartosci_pom
              (Przedzial p1)
              (zlacz_sume_przedzialow (przed, p2))
          else
            zlacz_wartosci_pom
              (Przedzial p1)
              (zlacz_sume_przedzialow (p2, przed))
      | SumaPrzedzialow (p1, p2), SumaPrzedzialow (p3, p4) ->
          zlacz_wartosci_pom
            (wartosc_od_do neg_infinity (max p1.max p3.max))
            (wartosc_od_do (min p2.min p4.min) infinity)
    in
    zlacz_wartosci_pom wart1 wart2


(** wykonuję dodawanie wartości *)
let plus wartosc1 wartosc2 =
  (* procedura pomocnicza dodawania bez sprawdzania 
    czy nan i oznakowania zera *)
  let dodawanie wart1 wart2 =
    match (wart1, wart2) with
    | Przedzial p1, Przedzial p2 ->
        let sum1 = p1.max +. p2.max
        and sum2 = p1.min +. p2.min in
        if czy_nan sum1 || czy_nan sum2
        then wartosc_nan
        else wartosc_od_do sum2 sum1
    | SumaPrzedzialow (p1, p2), Przedzial przed
    | Przedzial przed, SumaPrzedzialow (p1, p2) ->
        wartosc_do_od (przed.max +. p1.max) (przed.min +. p2.min)
    | SumaPrzedzialow _, SumaPrzedzialow _ ->
        wartosc_rzecz
  in
  oczysc wartosc1 wartosc2 dodawanie


(** wykonuję odejmowanie przedziałów *)
let minus wartosc1 wartosc2 = plus wartosc1 (przeciwnosc wartosc2)

(** wykonuję mnożenie przedziałów *)
let rec razy wartosc1 wartosc2 =
  (* procedura pomocnicza mnożenia wartości,
    ktorą nie sprawdza nan i nie oznakowuje zer *)
  let mnozenie wart1 wart2 =
    match (wart1, wart2) with
    | Przedzial p1, Przedzial p2 ->
        let mul1 = p1.min *.. p2.min
        and mul2 = p1.min *.. p2.max
        and mul3 = p1.max *.. p2.min
        and mul4 = p1.max *.. p2.max in
        let min_mul = min mul1 (min mul2 (min mul3 mul4))
        and max_mul = max mul1 (max mul2 (max mul3 mul4)) in
        wartosc_od_do min_mul max_mul
    | SumaPrzedzialow (p1, p2), Przedzial przed
    | Przedzial przed, SumaPrzedzialow (p1, p2) ->
        let mul1 = razy (Przedzial p1) (Przedzial przed)
        and mul2 = razy (Przedzial p2) (Przedzial przed) in
        if min_wartosc mul1 > min_wartosc mul2
        then zlacz_wartosci mul2 mul1
        else zlacz_wartosci mul1 mul2
    | SumaPrzedzialow (p1, p2), SumaPrzedzialow (p3, p4) ->
        zlacz_wartosci
          (zlacz_wartosci
             (razy (Przedzial p1) (Przedzial p4))
             (razy (Przedzial p1) (Przedzial p3)))
          (zlacz_wartosci
             (razy (Przedzial p2) (Przedzial p3))
             (razy (Przedzial p2) (Przedzial p4)))
  in
  oczysc wartosc1 wartosc2 mnozenie


(** zwraca odwrotnosc przedzialu jako wartość *)
let odwrotnosc_przedzialu p =
  if p.min = 0. && p.max = 0.
  then wartosc_nan
  else
    let div1 = 1. /. p.min
    and div2 = 1. /. p.max in
    let div_min = min div1 div2
    and div_max = max div1 div2 in
    match
      ( dodatnie_zero_w_wartosci (Przedzial p)
      , ujemne_zero_w_wartosci (Przedzial p) )
    with
    | false, false ->
        wartosc_od_do div_min div_max
    | true, false ->
        wartosc_od_do div_min infinity
    | false, true ->
        wartosc_od_do neg_infinity div_max
    | true, true ->
        wartosc_do_od div_min div_max


(** wykonuję dzielenie wartości jako mnożenie przez odwrotność *)
let podzielic wartosc1 wartosc2 =
  (* procedura pomocnicza, która zwraca odwrotnosć wartości  *)
  let odwrotnosc_wartosci _ wart =
    match wart with
    | Przedzial p ->
        odwrotnosc_przedzialu p
    | SumaPrzedzialow (p1, p2) ->
        zlacz_wartosci (odwrotnosc_przedzialu p1) (odwrotnosc_przedzialu p2)
  in
  let odw_wart2 = oczysc wartosc1 wartosc2 odwrotnosc_wartosci in
  razy wartosc1 odw_wart2

(* testy *)

(* let is_nan x = compare x nan = 0

let a = wartosc_od_do (-1.) 1. (* <-1, 1> *)

let b = wartosc_dokladna (-1.) (* <-1, -1> *)

let c = podzielic b a

(* (-inf -1> U <1 inf) *)

let d = plus c a

(* (-inf, inf) *)

let e = wartosc_dokladna 0. (* <0, 0> *)

let f = razy c e

(* <0, 0> *)

let g = razy d e

(* <0, 0> *)

let h = wartosc_dokladnosc (-10.) 50. (* <-15, -5> *)

let i = podzielic h e

(* nan, przedzial pusty*)

let j = wartosc_od_do (-6.) 5. (* <-6, 5> *)

let k = razy j j

(* <-30, 36> *)

let l = plus a b

(* <-2, 0> *)

let m = razy b l

(* <0, 2> *)

let n = podzielic l l

(* <0, inf) *)

let o = podzielic l m

(* (-inf, 0) *)

let p = razy o a

(* (-inf, inf) *)

let q = plus n o

(* (-inf, inf) *)

let r = minus n n

(* (-inf, inf) *)

let s = wartosc_dokladnosc (-0.0001) 100. (* <-0.0002, 0> *)

let t = razy n s

(* (-inf, 0) *)

;;
assert ((min_wartosc c, max_wartosc c) = (neg_infinity, infinity)) ;
assert (is_nan (sr_wartosc c)) ;
assert (not (in_wartosc c 0.)) ;
assert (
  in_wartosc c (-1.)
  && in_wartosc c (-100000.)
  && in_wartosc c 1.
  && in_wartosc c 100000. ) ;
assert (
  in_wartosc d 0.
  && in_wartosc d (-1.)
  && in_wartosc d (-100000.)
  && in_wartosc d 1.
  && in_wartosc d 100000. ) ;
assert ((min_wartosc f, max_wartosc f, sr_wartosc f) = (0., 0., 0.)) ;
assert ((min_wartosc g, max_wartosc g, sr_wartosc g) = (0., 0., 0.)) ;
assert ((min_wartosc h, max_wartosc h, sr_wartosc h) = (-15., -5., -10.)) ;
assert (
  is_nan (min_wartosc i) && is_nan (sr_wartosc i) && is_nan (max_wartosc i) ) ;
assert ((min_wartosc k, max_wartosc k, sr_wartosc k) = (-30., 36., 3.)) ;
assert ((min_wartosc n, max_wartosc n, sr_wartosc n) = (0., infinity, infinity)) ;
assert (
  (min_wartosc o, max_wartosc o, sr_wartosc o)
  = (neg_infinity, 0., neg_infinity) ) ;
assert (
  (min_wartosc p, max_wartosc p, is_nan (sr_wartosc p))
  = (neg_infinity, infinity, true) ) ;
assert (
  (min_wartosc q, max_wartosc q, is_nan (sr_wartosc q))
  = (neg_infinity, infinity, true) ) ;
assert (
  (min_wartosc r, max_wartosc r, is_nan (sr_wartosc r))
  = (neg_infinity, infinity, true) ) ;
assert (
  (min_wartosc t, max_wartosc t, sr_wartosc t)
  = (neg_infinity, 0., neg_infinity) )

let a = wartosc_od_do neg_infinity infinity

let c = plus a a

let d = razy a a

let e = podzielic a a

let f = minus a a

;;
assert (
  (min_wartosc c, max_wartosc c, is_nan (sr_wartosc c))
  = (neg_infinity, infinity, true) ) ;
assert (
  (min_wartosc d, max_wartosc d, is_nan (sr_wartosc d))
  = (neg_infinity, infinity, true) ) ;
assert (
  (min_wartosc e, max_wartosc e, is_nan (sr_wartosc e))
  = (neg_infinity, infinity, true) ) ;
assert (
  (min_wartosc d, max_wartosc d, is_nan (sr_wartosc d))
  = (neg_infinity, infinity, true) )

let a = wartosc_od_do 0. infinity

let b = wartosc_dokladna 0.

let c = podzielic a b

let d = podzielic b b

;;
assert (
  (is_nan (min_wartosc c), is_nan (max_wartosc c), is_nan (sr_wartosc c))
  = (true, true, true) ) ;
assert (
  (is_nan (min_wartosc d), is_nan (max_wartosc d), is_nan (sr_wartosc d))
  = (true, true, true) )

let a = wartosc_od_do (-10.) 10.

let b = wartosc_od_do (-1.) 1000.

let c = podzielic a b

;;
assert (
  (min_wartosc c, max_wartosc c, is_nan (sr_wartosc c))
  = (neg_infinity, infinity, true) )

let a = wartosc_od_do (-1.0) 1.0

let b = wartosc_dokladna 1.0

let c = podzielic b a

let d = wartosc_dokladna 3.0

let e = plus c d (* (-inf, 2> U <4 inf) *)

let f = podzielic b e (* (-inf, 1/4> U <1/2, inf) *)

let g = podzielic d a (* (-inf, -3> U <3, inf) *)

let h = podzielic g f (* (-inf, inf *)

let i = plus f g

(* (-inf, inf) *)

;;
assert (
  (in_wartosc f 0.25, in_wartosc f 0.26, in_wartosc f 0.49, in_wartosc f 0.50)
  = (true, false, false, true) ) ;
assert (
  (min_wartosc h, max_wartosc h, is_nan (sr_wartosc h), in_wartosc h 0.)
  = (neg_infinity, infinity, true, true) ) ;
assert (
  (min_wartosc h, max_wartosc h, is_nan (sr_wartosc h), in_wartosc h 0.3)
  = (neg_infinity, infinity, true, true) )

let jed = wartosc_dokladna 1.

let zero = wartosc_dokladna 0.

;;
assert ((sr_wartosc zero, max_wartosc zero, min_wartosc zero) = (0., 0., 0.))

let a = wartosc_od_do 0. 1. (* <0,1> *)

let b = podzielic a a (* <0, inf)*)

let c = razy b zero

(* <0,0> *)

;;
assert ((sr_wartosc c, max_wartosc c, min_wartosc c) = (0., 0., 0.))

let a = podzielic jed zero

(* nan *)

;;
assert (is_nan (min_wartosc a)) ;
assert (is_nan (max_wartosc a)) ;
assert (is_nan (sr_wartosc a))

let a = wartosc_dokladnosc 1. 110.

(* <-0.1, 2.1> *)

;;
assert (in_wartosc a (-0.1)) ;
assert (in_wartosc a 2.1)

let a = wartosc_od_do (-3.) 0. (* <-3.0, 0.0> *)

let b = wartosc_od_do 0. 1. (* <-0.0, 1.0> *)

let c = podzielic a b

(* (-inf, 0> *)

;;
assert (max_wartosc c = 0.) ;
assert (min_wartosc c = neg_infinity) ;
assert (sr_wartosc c = neg_infinity)

let a = wartosc_od_do 1. 4. (* <1.0, 4.0> *)

let b = wartosc_od_do (-2.) 3. (* <-2.0, 3.0> *)

let c = podzielic a b (* (-inf, -1/2> U <1/3, inf) *)

let d = podzielic c b (* (-inf, -1/6> U <1/9, inf) *)

let e = plus d jed (* (-inf, 5/6> U <10/9, inf) *)

let f = sr_wartosc (podzielic jed (wartosc_dokladna 9.))

(* 1/9 *)

;;
assert (is_nan (sr_wartosc d)) ;
assert (in_wartosc d 0.12) ;
assert (not (in_wartosc d 0.)) ;
assert (not (in_wartosc d (-0.125))) ;
assert (in_wartosc d f) ;
assert (not (in_wartosc e 1.))

(* uwaga, ten test moze sie zawiesic przy pewnych implementacjach! *)
let a = wartosc_od_do (-2.) 3.

let b = wartosc_od_do 2. 3.

let c = podzielic b a

let rec iteruj f n acc =
  match n with
  | 0 ->
      acc
  | n when n > 0 ->
      iteruj f (n - 1) (f acc acc)
  | _ ->
      acc


let x = iteruj razy 10 c

;;
assert (not (in_wartosc x 0.)) *)
