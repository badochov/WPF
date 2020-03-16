(* lista głów w liscie list *)
let heads list_of_lists =
  let filterred = List.filter (fun el -> el <> []) list_of_lists in
  List.map (fun l -> List.hd l) filterred
