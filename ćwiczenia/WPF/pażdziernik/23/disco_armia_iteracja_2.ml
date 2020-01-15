(* każdy  *)
type armia = Node of int * armia list

let rec imprezownosc_czlonka (Node (imprezowosc, dzieci)) =
  let wyniki = List.map imprezownosc_czlonka dzieci in
  let f (z_sb, bez_sb) (z_dziecmi, bez_dzieci) =
    (z_sb + z_dziecmi, bez_sb + max z_dziecmi bez_dzieci)
  in
  let z, bez = List.fold_left f (0, 0) wyniki in
  (imprezowosc + z, bez)


let przyjecie armia =
  let z, bez = imprezownosc_czlonka armia in
  max z bez
