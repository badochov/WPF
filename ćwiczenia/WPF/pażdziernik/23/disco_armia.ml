(* każdy  *)
type armia = Node of int * armia list

let rec imprezownosc_czlonka t =
  let rec imprezownosc_podwladnych l acc =
    match l with
    | [] ->
        acc
    | podwladny :: reszta ->
        let z_sb, bez_sb = acc in
        let z_podwladnymi, bez_podwladnych = imprezownosc_czlonka podwladny in
        imprezownosc_podwladnych
          reszta
          (z_sb + z_podwladnymi, bez_sb + max z_podwladnymi bez_podwladnych)
  in
  match t with
  | Node (imprezownosc, podwladni) ->
      let z_ojcem, bez_ojca = imprezownosc_podwladnych podwladni (0, 0) in
      (imprezownosc + z_ojcem, bez_ojca)


let przyjecie armia =
  let z, bez = imprezownosc_czlonka armia in
  max z bez
