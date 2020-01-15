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

(* Na szachownicy jest ustawionych n wież, które należy pokolorować. Jeśli dwie
   wieże się atakują (są w tej samej kolumnie lub wierszu), to muszą być tego
   samego koloru. Napisz procedurę kolory: int * int list → int, która na
   podstawie listy współrzędnych wież wyznaczy maksymalną liczbę kolorów,
   których można użyć kolorując wieże. Podaj złożoność czasową i pamięciową
   swojego rozwiązania. *)

let rozm =
  List.fold_left
    (fun (max_x, max_y) (x, y) -> (max max_x x, max max_y y))
    (0, 0)


let kolory l =
  let module Fu = FU () in
  let podlacz tab i zbior =
    match tab.(i) with
    | None ->
        tab.(i) <- Some zbior
    | Some x ->
        Fu.union x zbior
  in
  let dodaj tab_x tab_y (x, y) =
    let z = Fu.make_set (x, y) in
      podlacz tab_x x z; podlacz tab_y y z
  in
  let (rozm_x, rozm_y) = rozm l in
  let tab_x = Array.make (rozm_x + 1) None in
  let tab_y = Array.make (rozm_y + 1) None in
    List.iter (dodaj tab_x tab_y) l;
    Fu.n_of_sets
