(* 
    Na szachownicy n×n należy ustawić n wież tak, żeby żadne dwie się nie szachowały, a
    ponadto każda z tych wież ma wyznaczony prostokąt, w którym ma stać.
    Znajdź takie ustawienie, lub stwierdź, że się nie da.
*)

exception Nie_da_sie

let wieze prostokaty =
  let n = Array.length prostokaty in
  (* problem jednowymiarowy (kolumny lub wiersze)*)
  let problem_1d prz =
    let wym = Array.make n 0 in
      let rec petla l q i = 
       let wez () = 
        if PQ.is_empty q then raise Nie_da_sie
        else 
            let (q,(k,j)) = PQ.delete_min q in
                if k<i then raise Nie_da_sie
                else wym.(j) <- i;
                petla l q (i+1)
        in match l with
        | [] -> wez ()
        | ((p,k),j)::t -> 
            if i = k then petla t (PQ.add (k,j)) i
            else wez ()
  in
    (petla (let a = ))
