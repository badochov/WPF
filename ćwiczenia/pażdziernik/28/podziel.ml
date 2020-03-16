(* rozdziela liste na liste list tak, że kiedy obok siebie są dwa takie same elementy 
    podziel [1;1;2;3;3;4] = [[1]; [1; 2; 3]; [3; 4]] *)

let podziel l =
  let pom (l1 :: t) el =
    if el = List.hd l1 then [el] :: l1 :: t else (el :: l1) :: t
  in
  match List.rev l with
  | [] ->
      []
  | head :: tail ->
      List.fold_left pom [[head]] tail

let () = assert (podziel [1; 1; 2; 3; 3; 4] = [[1]; [1; 2; 3]; [3; 4]])
