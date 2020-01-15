(* dołączyć l2 na koniec l1 *)
let append l1 l2 =
  let add el acc = el :: acc in
  List.fold_right add l1 l2

;;
append [1; 2] [3; 4]
