let podziel liczba_el_w_paczce l =
  let rec pom ll lp lw =
    match (lp, lw) with
    | _, [] ->
        List.rev ll @ lp
    | [], _ ->
        pom [] (List.rev ll) lw
    | lista :: reszta, x :: t ->
        pom ((x :: lista) :: ll) reszta t
  and zrob n el =
    let rec rec_pom i el acc =
      if i <= 0 then acc else rec_pom (i - 1) el (el :: acc)
    in
    rec_pom n el []
  in
  List.map List.rev (pom [] (zrob liczba_el_w_paczce []) (List.rev l))

let podzielone = podziel 3 [0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10]
