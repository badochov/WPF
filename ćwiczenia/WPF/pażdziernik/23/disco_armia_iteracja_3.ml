(* każdy  *)
type armia = Node of int * armia list

let rec imprezownosc_czlonka (Node (imprezowosc, dzieci)) =
  let imprezowosc_dziecko (z_sb, bez_sb) dziecko =
    let z_dzieckiem, bez_dziecka = imprezownosc_czlonka dziecko in
    (z_sb + z_dzieckiem, bez_sb + max z_dzieckiem bez_dziecka)
  in
  List.fold_left imprezowosc_dziecko (imprezowosc, 0) dzieci


let przyjecie armia =
  let z, bez = imprezownosc_czlonka armia in
  max z bez
