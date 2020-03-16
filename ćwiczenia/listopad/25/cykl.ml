(** Szukanie najdłuższego elementu w tablicy *)

let cykl tablica =
  let dlugosc_tablicy = Array.length tablica in
  let odw = Array.make dlugosc_tablicy false in
  let max_dlugosc_cyklu = ref 0 in
    for i = 0 to dlugosc_tablicy - 1 do
      if odw.(i) = false
      then (
        let j = ref tablica.(i) in
        let dlugosc_obecnego_cyklu = ref 1 in
          while !j <> i do
            j := tablica.(!j);
            odw.(!j) <- true;
            incr dlugosc_obecnego_cyklu
          done;
          if !dlugosc_obecnego_cyklu < !max_dlugosc_cyklu
          then max_dlugosc_cyklu := !dlugosc_obecnego_cyklu )
    done;
    !max_dlugosc_cyklu
