(* wyjątek rzucany kiedy znaleziony zostaje element spełniający predykat *)
exception Done

(* sprawdza czy element istnieje w liście, który spełnia predykat *)
let exists pred l =
  let exists_helper p el = if p el then raise Done else () in
  try
    List.iter (exists_helper pred) l ;
    false
  with Done -> true

(* tworzy predykat przeciwny predykatowi *)
let non p x = not (p x)

(* sprawdza czy wszystkie elementy listy spełniaja predykat *)
(* można usunąć l bo i tak bedzie przekazywane przy wyołaniu 2 argumentowym *)
let for_all pred l = not (exists (non pred) l)
