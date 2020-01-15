(* 
    Na rozgałęzieniach drzewa siedzą wróble.
    Każde rozgałęzienie drzewa waży 1 kilogram, a nieujemna waga siedzących na tym rozgałęzieniu wróbli podana jest w drzewie:
    
        type drzewo = Leaf | Node of int * drzewo * drzewo
    
    Waga drzewa to suma wag rozgałęzień i siedzących na nich wróbli.
    Drzewo nazwiemy zrównoważonym, jeśli w każdym rozgałęzieniu waga dwóch poddrzew będzie taka sama.
    Rzucając celnie kamieniem w rozgałęzienie drzewa możemy przegonić wszystkie wróble z poddrzewa zaczepionego w tym rozgałęzieniu.
    Napisz funkcję rownowaga : drzewo → bool, która dla danego drzewa określa, czy da się tak rzucać kamieniami w drzewo,
    aby stało się ono zrównoważone. Podaj złożoność czasową i pamięciową swojego rozwiązania.

    Dla przykładu, aby zrównoważyć następujące drzewo:

    
        Node (3, Node (5, Node (1, Leaf, Leaf), Node (7, Leaf, Leaf)), Node (2, Leaf, Leaf))
    
    wystarczy raz rzucić kamieniem (w jego lewe poddrzewo).
 *)

type tree =
  | Leaf
  | Node of int * tree * tree

exception Unbalancable
