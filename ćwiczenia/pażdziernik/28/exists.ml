exception Koniec

let istnieje predykat lista =
  let istnieje_pom p _ el = if p el then raise Koniec else false in
  try List.fold_left (istnieje_pom predykat) false lista with Koniec -> true
