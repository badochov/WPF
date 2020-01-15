(* Jaka jest minimalna liczba monet lub banknotów potrzebna do wydania nzł reszty, przyjmując, że w obrocie są dostępne monety o zadanych (w postaci tablicy) nominałach? 
    lub max_int jeżeli sie nie da *)
module IMap = Map.Make (Int)

let monety suma nominaly =
  let pamiec = ref IMap.empty in
    pamiec := IMap.add 0 0 !pamiec;
    let rec dynamik wartosc =
      if IMap.mem wartosc !pamiec
      then IMap.find wartosc !pamiec
      else
        let get_min acc nominal =
          if wartosc < nominal
          then acc
          else min acc (dynamik (wartosc - nominal))
        in
        let res = 1 + List.fold_left get_min (max_int - 1) nominaly in
          pamiec := IMap.add wartosc res !pamiec;
          res
    in
      dynamik suma


let () =
  assert (monety 1 [1] = 1);
  assert (monety 3 [1; 2] = 2);
  assert (monety 2 [1] = 2)


(* Na ile sposobów można wydać suma zł reszty przyjmując, że w obrocie są dostępne monety o zadanych (w postaci listy) nominałach. *)
let monety_sposoby suma nominaly = 0
