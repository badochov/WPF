(* sums [x1; ...; xn] = [x1; x1 + x2 ; ... ; x1 + ... + xn] *)

let sums l : int list =
  let rev_list =
    List.fold_left
      (fun acc el ->
        let prev = if acc = [] then 0 else List.hd acc in
        (prev + el) :: acc)
      [] l
  in
  List.rev rev_list

let test = sums [0; 1; 2; 4; 5]
