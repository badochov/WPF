(* Autor Hubert Badocha *)
(* Code review Jakub Moliński *)

(** typ trzymający drzewo lewicowe 
    w formie Leaf -> koniec danej gałęzi
    lub Node (wartość, wysokość prawego(minimalnego poddrzewa),
    lewe poddrzewo, lewe poddrzewo)*)
type 'a queue = Leaf | Node of 'a * int * 'a queue * 'a queue

(* Wyjątek podnoszony przez [delete_min] gdy kolejka jest pusta *)
exception Empty

(** zwraca wysokośc drzewa lub -1 jeśli to Leaf *)
let get_tree_height tree =
  match tree with Leaf -> -1 | Node (_, h, _, _) -> h

(** sortuje rosnącą drzewa według wysokości prawego poddrzewa
    przyjmuje i zwraca parę drzew  *)
let sort_trees_by_min_height (tree1, tree2) =
  if get_tree_height tree1 > get_tree_height tree2 then (tree1, tree2)
  else (tree2, tree1)

(** procedura służąca do łączenia drzew lewicowych,
    przyjmuje 2 drzewa i zwraca 1 *)
let rec join tree1 tree2 =
  match (tree1, tree2) with
  | Leaf, tree | tree, Leaf ->
      tree
  | (Node (v1, _, _, _) as n1), (Node (v2, _, _, _) as n2) when v1 > v2 ->
      join n2 n1
  | Node (v, _, lt, rt), (Node _ as n2) ->
      let merged_tree = join rt n2 in
      let left_tree, right_tree = sort_trees_by_min_height (lt, merged_tree) in
      let height = get_tree_height right_tree + 1 in
      Node (v, height, left_tree, right_tree)

(** Zwraca [true] jeśli dana kolejka jest pusta. W przeciwnym razie [false] *)
let is_empty tree = tree = Leaf

(** Dla niepustej kolejki [q], [delete_min q] zwraca parę [(e,q')] gdzie [e]
    jest elementem minimalnym kolejki [q] a [q'] to [q] bez elementu [e].
    Jeśli [q] jest puste podnosi wyjątek [Empty]. *)
let delete_min tree =
  match tree with
  | Leaf ->
      raise Empty
  | Node (min_el, _, l_tree, r_tree) ->
      (min_el, join l_tree r_tree)

(** Pusta kolejka priorytetowa *)
let empty = Leaf

(** [add e q] zwraca kolejkę powstałą z dołączenia elementu [e] 
    do kolejki [q] *)
let add el tree = join (Node (el, 0, Leaf, Leaf)) tree

(* 
(* simple tests *)
let a = empty;;
let b = add 1 empty;;

assert (is_empty a = true);;
assert (try let _=delete_min a in false with Empty -> true);;
assert (is_empty b <> true);;

let b = join a b ;;
assert (is_empty b <> true);;

let (x,y) = delete_min b;;

assert (x = 1);;
assert (is_empty y = true);;
assert (try let _=delete_min y in false with Empty -> true);;

(* delete_min integer tests *)
let b = add 1 empty;;
let b = add 3 b;;
let b = add (-1) b;;
let b = add 2 b;;
let b = add 1 b;;

let (a,b) = delete_min b;;
assert (a = -1);;

let (a,b) = delete_min b;;
assert (a = 1);;

let (a,b) = delete_min b;;
assert (a = 1);;

let (a,b) = delete_min b;;
assert (a = 2);;

let (a,b) = delete_min b;;
assert (a = 3);;

assert(is_empty b = true);;

(* delete_min string tests *)
let b = add "a" empty;;
let b = add "aca" b;;
let b = add "nzbzad" b;;
let b = add "nzbza" b;;
let b = add "bxbxc" b;;

let (a,b) = delete_min b;;
assert (a = "a");;

let (a,b) = delete_min b;;
assert (a = "aca");;

let (a,b) = delete_min b;;
assert (a = "bxbxc");;

let (a,b) = delete_min b;;
assert (a = "nzbza");;

let (a,b) = delete_min b;;
assert (a = "nzbzad");;

assert(is_empty b = true);;
assert (try let _=delete_min b in false with Empty -> true);;

(* join tests *)

let b = add 1 empty;;
let b = add 3 b;;
let b = add (-1) b;;
let b = add 2 b;;
let b = add 1 b;;

let c = add 10 empty;;
let c = add (-5) c;;
let c = add 1 c;;
let c = add 4 c;;
let c = add 0 c;;

let b = join b c;;

let (a,b) = delete_min b;;
assert (a = (-5));;

let (a,b) = delete_min b;;
assert (a = (-1));;

let (a,b) = delete_min b;;
assert (a = 0);;

let (a,b) = delete_min b;;
assert (a = 1);;

let (a,b) = delete_min b;;
assert (a = 1);;

let (a,b) = delete_min b;;
assert (a = 1);;

let (a,b) = delete_min b;;
assert (a = 2);;

let (a,b) = delete_min b;;
assert (a = 3);;

let (a,b) = delete_min b;;
assert (a = 4);;

let (a,b) = delete_min b;;
assert (a = 10);;

assert (try let _=delete_min b in false with Empty -> true);;

let b = add 1 empty;;
let b = add 3 b;;
let b = add (-1) b;;
let b = add 2 b;;
let b = add 1 b;;

let c = add 10 empty;;
let c = add (-5) c;;
let c = add 1 c;;
let c = add 4 c;;
let c = add 0 c;;

let b = join c b;;

let (a,b) = delete_min b;;
assert (a = (-5));;

let (a,b) = delete_min b;;
assert (a = (-1));;

let (a,b) = delete_min b;;
assert (a = 0);;

let (a,b) = delete_min b;;
assert (a = 1);;

let (a,b) = delete_min b;;
assert (a = 1);;

let (a,b) = delete_min b;;
assert (a = 1);;

let (a,b) = delete_min b;;
assert (a = 2);;

let (a,b) = delete_min b;;
assert (a = 3);;

let (a,b) = delete_min b;;
assert (a = 4);;

let (a,b) = delete_min b;;
assert (a = 10);;

assert (try let _=delete_min b in false with Empty -> true);;
 *)
