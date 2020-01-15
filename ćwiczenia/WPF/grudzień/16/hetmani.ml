(* ile minimalnie trzeba postawić hetmanów na planszy n*n żeby wszystkie pola atakowały *)

module type IntPairOdrderSig = sig
  type t = int * int

  val compare : t -> t -> int
end

type pos = int * int

module IntPairOrder : IntPairOdrderSig = struct
  type t = pos

  let compare = compare
end

module IntPairOrderSet = Set.Make (IntPairOrder)

(* typ planszy int to liczba niebitych pól *)
type board = IntPairOrderSet.t * int

(* zwraca pozycje kolejnego hetmana przyjmuje obecną pozycje i rozmiar planszy *)
let next_pos n = function
  | None ->
      Some (0, 0)
  | Some (x, y) ->
      if y = n - 1
      then if x = n - 1 then None else Some (x + 1, 0)
      else Some (x, y + 1)


let put position board = board

let rec next
    n
    board
    ((count, position_list) as curr)
    ((min_count, min_position_list) as mini)
    position =
  if n + 1 >= min_count
  then mini
  else
    match next_pos n position with
    | None ->
        mini
    | Some pos ->
        let new_board = put pos board in
          if board = new_board
          then next n board curr mini (Some pos)
          else if snd new_board = 0
          then (count + 1, pos :: position_list)
          else
            let new_mini =
              next n new_board
                (count + 1, pos :: position_list)
                mini (Some pos)
            in
              next n board curr new_mini (Some pos)


let good_answer n =
  if n = 1 || n = 2 then [(0, 0)] else if n = 3 then [(1, 1)] else []


let hetminy n =
  let empty_board = (IntPairOrderSet.empty, n * n) in
    next n empty_board (0, []) (n - 2, good_answer n)
