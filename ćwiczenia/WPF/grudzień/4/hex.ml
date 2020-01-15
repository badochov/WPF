(* http://smurf.mimuw.edu.pl/wstep_do_programowania_funkcyjny?q=wstep_do_programowania_funkcyjny_cw_17 zadanie 9 *)
module type FIND_AND_UNION = sig
  type 'a set

  val make_set : 'a -> 'a set

  val find : 'a set -> 'a

  val equivalent : 'a set -> 'a set -> bool

  val union : 'a set -> 'a set -> unit

  val elements : 'a set -> 'a list

  val n_of_sets : unit -> int
end

module FU () : FIND_AND_UNION = struct
  type 'a set = {a : 'a}

  let make_set x = {a = x}

  let find {a} = a

  let equivalent s1 s2 = true

  let union s1 s2 = ()

  let elements s1 = []

  let n_of_sets _ = 0
end

exception Wygrana of (bool * int)

let sasiedzi = [(-1, -1); (0, -1); (-1, 0); (1, 0); (0, 1); (1, 1)]

let hex n ruchy =
  let module Fu = FU () in
  let plansza = Array.make_matrix (n + 2) (n + 2) None in
  let lewa = Fu.make_set (0, n) in
  let prawa = Fu.make_set (n + 1, n) in
  let gora = Fu.make_set (n, 0) in
  let dol = Fu.make_set (n, n + 1) in
  (* wykonuje ruch przyjmje numer ruchu i współrzędne gdzie ląduje pionek *)
  let ruch num (x, y) =
    let bialy = num mod 2 = 0 in
    let set = Fu.make_set (x, y) in
      List.iter
        (fun (i, j) ->
          match plansza.(x + i).(y + j) with
          | Some (b, s) when b = bialy ->
              Fu.union set s
          | _ ->
              ())
        sasiedzi;
      if bialy
      then (if Fu.equivalent prawa lewa then raise (Wygrana (bialy, num)))
      else if Fu.equivalent gora dol
      then raise (Wygrana (bialy, num));
      num + 1
  in
    for i = 1 to n do
      plansza.(0).(i) <- Some (true, lewa);
      plansza.(n + 1).(i) <- Some (true, lewa);
      plansza.(i).(0) <- Some (true, lewa);
      plansza.(i).(n + 1) <- Some (true, lewa)
    done;
    try
      let _ = List.fold_left ruch 0 ruchy in
        (true, -1)
    with
    | Wygrana (wygrany, liczba_ruchow) ->
        (wygrany, liczba_ruchow)
