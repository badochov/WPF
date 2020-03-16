(* zwraca co drugi element listy *)

let every_second l =
  let f (parity, acc) el =
    if parity then (false, acc) else (true, el :: acc)
  in
  let _, rev_list = List.fold_left f (false, []) l in
  List.rev rev_list

let () =
  assert (every_second [0; 1; 2; 3; 4] = [0; 2; 4]) ;
  assert (every_second [0; 1] = [0]) ;
  assert (every_second [] = [])
