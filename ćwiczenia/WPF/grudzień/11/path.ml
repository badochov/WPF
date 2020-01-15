(* 
    Dany jest graf nieskierowany, którego wierzchołki są ponumerowane od 0 do n−1.
    Napisz procedurę path:graph→int, która wyznacza długość (liczoną liczbą wierzchołków) najdłuższej ścieżki w tym grafie, na której numery wierzchołków tworzą ciąg rosnący.
 *)
type graph = (int * int list) array

let rec dfs vertex dist graph =
  if dist.(vertex) = 0
  then
    dist.(vertex) <-
      List.fold_left
        (fun acc v ->
          if fst graph.(v) > fst graph.(vertex) then dfs v dist graph else 0)
        0
        (snd graph.(vertex))
      + 1;
  dist.(vertex)


let path g =
  let n = Array.length g in
  let dist = Array.make n 0 in
  let longest_path = ref 0 in
    for i = 0 to n - 1 do
      longest_path := max !longest_path (dfs i dist g)
    done;
    !longest_path


let () =
  assert (path [|(1, [1]); (2, [])|] = 2);
  assert (path [||] = 0);
  assert (path [|(1, [])|] = 1)
