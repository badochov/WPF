(** Autor: Hubert Badocha *)

(** Code review: Wojciech Przytuła *)

(* rozdziela dane wejściowe na oczekiwane i maksymalne objętości *)
let split arr =
  let n = Array.length arr in
    (Array.init n (fun i -> fst arr.(i)), Array.init n (fun i -> snd arr.(i)))

(* wyjątek rzucany w przypadku znalezienia najkrótszej ścieżki do żądanej
   kombinacji, jego wartość to liczba potrzebnych operacji *)
exception Found of int

(* Sprawdza czy podany stan jeszcze nie wystąpił oraz czy nie jest stanem
   wyjściowym, jeżeli nie wystąpił jest wrzucany do kolejki jeżeli jest stanem
   oczekiwanym rzucany jest wyjatek Found *)
let process_state (state, num) goal vis q =
  if not (Hashtbl.mem vis state)
  then
    if state = goal
    then raise (Found num)
    else (
      Queue.add (state, num) q;
      Hashtbl.add vis state true )

(* generuje stan powstały po wylaniu wody z i-tej szklanki, nastepnie odpala na
   nim funkcje process_state *)
let spill i (state, num) goal vis q =
  if state.(i) > 0
  then (
    let temp = Array.copy state in
      temp.(i) <- 0;
      process_state (temp, num + 1) goal vis q )

(* generuje stany powstałe po przelaniu wody z i-tej szklanki do każdej innej
   szkalnki, nastepnie odpala na nich funkcje process_state *)
let pour i (state, num) vol goal vis q =
  if state.(i) <> 0
  then
    for j = 0 to Array.length state - 1 do
      if i <> j && state.(j) <> vol.(j)
      then (
        let temp = Array.copy state in
          temp.(j) <- min vol.(j) (temp.(j) + temp.(i));
          temp.(i) <- temp.(i) - (temp.(j) - state.(j));
          process_state (temp, num + 1) goal vis q )
    done

(* generuje stan powstały po napełnieniu wody z i-tej szklanki, nastepnie
   odpala na nim funkcje process_state *)
let fill i (state, num) vol goal vis q =
  if state.(i) < vol.(i)
  then (
    let temp = Array.copy state in
      temp.(i) <- vol.(i);
      process_state (temp, num + 1) goal vis q )

(* generuje nowe stany i wrzuca je do kolejki, sprawdza czy wygenerowany stan
   jeszcze nie wystąpił i czy nie jest stanem oczekiwanym

   Przyjmuje: (stan, liczba kroków), tablica z maksymalnymi pojemnościami, mapę
   odwiedzeń, stan porządany, kolejkę kolejnych stanów *)
let add_new_states ((state, num) as el) vol vis goal q =
  for i = 0 to Array.length state - 1 do
    pour i el vol goal vis q;
    fill i el vol goal vis q;
    spill i el goal vis q
  done

(* wyjątek rzucany w przypadku znalezienia szklanki, której objętośc zadana
   jest taka sama jej objętośc maksymalna *)
exception Full_found

(* sprawdza czy istnieje co najmniej jedna szkalnka pełna lub pusta *)
let has_full_or_empty vol goal =
  if Array.exists (fun el -> el = 0) goal
  then true
  else
    try
      let n = Array.length goal in
        for i = 0 to n - 1 do
          if goal.(i) = vol.(i) then raise Full_found
        done;
        false
    with
    | Full_found ->
        true

(* Sprawdza czy każda oczekiwana objetość jest podzielna przez nwd z objętości
   szklanek *)
let divisible vol goal =
  (* oblicza nwd dwóch liczb *)
  let rec gcd a b = if b = 0 then a else gcd b (a mod b) in
  (* oblicza nwd z objętości szklanek *)
  let vol_gcd =
    Array.fold_left
      (fun acc el -> if el = 0 then acc else gcd acc el)
      vol.(0) vol
  in
    if vol_gcd = 0
    then true
    else Array.for_all (fun el -> el mod vol_gcd = 0) goal

(* Sprawdza czy jest szansa na wypełnienie szklanek w zadany sposób *)
let possible vol goal =
  Array.length vol = 0 || (has_full_or_empty vol goal && divisible vol goal)

(* Zwraca minimalną ruchów żeby dojść do podanego stanu napełnienia szklanek,
   lub -1 jesli nie jest to możliwe *)
let przelewanka input =
  let (vol, goal) = split input in
    if possible vol goal
    then
      let q = Queue.create () in
      let vis = Hashtbl.create 42 in
        try
          process_state (Array.make (Array.length vol) 0, 0) goal vis q;
          while not (Queue.is_empty q) do
            let curr = Queue.pop q in
              add_new_states curr vol vis goal q
          done;
          -1
        with
        | Found ans ->
            ans
    else -1

(* testy *)

(* (*Nie ma rozwiązania*) let c = [|(10,2);(20,20);(10,0);(1000,1000)|];;
   assert ( przelewanka c = -1 );; let c = [|(3,2);(5,4);(5,2);(6,1)|];; assert
   (przelewanka c = -1);; let c = [|(40,1);(10,4);(23,2);(40,1)|];; assert
   (przelewanka c = -1);; let c = [|(12,2);(6,3);(4,4);(10,2)|];; assert
   (przelewanka c = -1);; let c = [|(14,3);(3,1)|];; assert (przelewanka c =
   -1);;

   (*Testy różne*) let c = [|(3,2);(3,3);(1,0);(12,1)|];; assert ( przelewanka
   c = 4 );; let c = [|(1,1);(100,99)|];; assert ( przelewanka c = 2 );; let c
   = [|(3,3);(5,4);(5,2);(6,1)|];; assert (przelewanka c = 6);; let c =
   [|(100,3);(2,1);(1,0);(6,1)|];; assert (przelewanka c = 7);; let c =
   [|(3,3);(5,5);(5,5);(6,6)|];; assert (przelewanka c = 4);; let c =
   [|(40,20);(20,10);(10,5);(5,0)|];; przelewanka c ;; let c =
   [|(19,3);(1,1);(2,2)|];; assert (przelewanka c = 6);; let c =
   [|(14,3);(3,1);(3,0)|];; assert (przelewanka c = 13);; print_string "xd";;
   let c = [|(3,3);(4,0);(1,1);(6,6)|];; assert (przelewanka c = 3);; let c =
   [|(46,20);(23,10);(13,5);(5,0)|];; assert (przelewanka c = 10);; let c =
   [|(18,3);(3,1);(2,2)|];; assert (przelewanka c = 4);; let c =
   [|(14,3);(5,1)|];; assert (przelewanka c = -1);; print_string "lol";; let c
   = [|(14,3);(5,1);(5,0)|];; assert (przelewanka c = 16);; *)
