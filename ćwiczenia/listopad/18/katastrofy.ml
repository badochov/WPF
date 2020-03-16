(** 
    Dla każdego elementu listy przypisać od ilu poprzednich jest większy
*)

let rec f s ((x, i) as el) =
  match s with
  | [] ->
      ([el], i + 1)
  | (v, last) :: t ->
      if v <= x then f t el else (el :: s, i - last)

let pom (res, stos, i) x =
  let nowy_stos, k = f stos (x, i) in
  (k :: res, nowy_stos, i + 1)

let katastrofy l =
  let res, _, _ = List.fold_left pom ([], [], 0) l in
  List.rev res
