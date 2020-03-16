let trojki l = 
  let rec petla_c a b l_c acc =
    match l_c with
    | [] -> acc
    | c::reszta ->
      if a + b < c
      then acc
      else petla_c a b reszta ((a,b,c)::acc)
  and
    petla_b a l_b acc =
    match l_b with
    | [] -> acc
    | b::reszta -> petla_b a reszta (petla_c a b reszta acc)
  and

    petla_a l_a acc = 
    match l_a with
    | [] -> acc
    | a::reszta -> petla_a reszta (petla_b a reszta acc)

  in
  petla_a l []

let _ = trojki [1; 2; 3]