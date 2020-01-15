(** Autor: Hubert Badocha *)

(** Code review: Franciszek Biel *)

(** wyjątek rzucany przez [topol] gdy zależności są cykliczne *)
exception Cykliczne

(** 
    Wartości które są w mapie odwiedzeń
*)
type visited_status =
  | Virgin
  | Visited_In_Cycle
  | Visited

(** 
    Dodaje do Mapy klucz o podanej wartości o ile klucz jeszcze nie istnieje w mapie
*)
let add_if_not_existing key value p_map =
  if PMap.mem key p_map then p_map else PMap.add key value p_map


(** Zwraca wartość przypisaną do klucza w p_map 
    lub podaną wartośc jeśli klucz nie należy do p_map *)
let find_or_default key p_map default =
  try PMap.find key p_map with Not_found -> default


(** 
    Dodaje krawędź.
    Przyjmuję:
        Wierzchołek z którego krawędzie wychodzą
        (Listę dotychczasowych krawedzi, dotychczsową mapę odwiedzeń)
    Zwraca listę krawędzi po wstawienu nowej 
    i wstawia wierzchołek na końcu krawędzi do mapy odwiedzeń.
    Rzuca Cykliczne jeśli krawedź jest z wierzchołku do samego siebie 
*)
let add_edge vertex (edges, vis) new_edge =
  if new_edge = vertex
  then raise Cykliczne
  else (new_edge :: edges, PMap.add new_edge Virgin vis)


(** Dodaje do mapy krawędzi krawędzie wychodzace z danego wierzchołka 
    oraz do mapy odwiedzień wszystkie napotkane wierzchołki *)
let add_edges_from_vertex (e_map, vis) (vertex, e_list) =
  let prev_e = find_or_default vertex e_map [] in
  let (all_e, vis) =
    List.fold_left (add_edge vertex)
      (prev_e, PMap.add vertex Virgin vis)
      e_list
  in
    (PMap.add vertex all_e e_map, vis)


(** 
    Tworzy mapę z krawędzi wyjściowych 
    i mape mówiącą czy dany wierzchołek był juz odwiedzony
    Mapa krawędzi wyjściowych ma formę
      wartość w wierzchołku -> lista wierzchołków do których wychodzą krawędzie
        
    Mapa mówiąca czy wierzchołek był odwiedzony ma formę
      wartość w wierzchołku -> Virgin

*)
let map_outcoming_edges edges_list =
  List.fold_left add_edges_from_vertex (PMap.empty, PMap.empty) edges_list


(** 
    Funkcja pomocnicza wykonująca rekurencje podobą do DFS.
    Przyjmuje wartość w wierzchołku,
    Argument typu list który ignoruje,
    Krotke:
        Lista juz posortowanych,
        mapę krawędzi z wierzchoków, które jeszcze nie były przeiterowane
        mape mówiącą o stanie poszczegołnych wierzchołków
*)
let rec pseudo_dfs_helper vertex _ (l, out_e, vis) =
  let status = find_or_default vertex vis Visited in
    if status = Visited
    then (l, out_e, vis)
    else if status = Visited_In_Cycle
    then raise Cykliczne
    else
      let vis = PMap.add vertex Visited_In_Cycle vis in
      let (l, _, vis) =
        List.fold_left
          (fun acc x -> pseudo_dfs_helper x [] acc)
          (l, out_e, vis)
          (find_or_default vertex out_e [])
      in
        (vertex :: l, out_e, PMap.remove vertex vis)


(**
    Wykonuje operaacja podobną do DFS przeszukując graf.
    Przyjmuje 
        mape krawędzi w formie:
            wierchołek -> lista krawwedzi do których dany wierzchołek wychodzi
        mapę mówiącą, które wierzchołki były dowiedzone i jesli tak to czy w tym cyklu
 *)
let pseudo_dfs edges_map visited =
  PMap.foldi pseudo_dfs_helper edges_map ([], edges_map, visited)


(** Dla danej listy [(a_1,[a_11;...;a_1n]); ...; (a_m,[a_m1;...;a_mk])] zwraca
    liste, na ktorej kazdy z elementow a_i oraz a_ij wystepuje dokladnie raz i
    ktora jest uporzadkowana w taki sposob, ze kazdy element a_i jest przed
    kazdym z elementow a_i1 ... a_il *)
let topol edges =
  let (outcoming_edges, visited) = map_outcoming_edges edges in
  let (sorted_verticies, _, visited) = pseudo_dfs outcoming_edges visited in
    if PMap.is_empty visited then sorted_verticies else raise Cykliczne

(* Testy
let print_p_map f1 f2 m =
  PMap.iter (fun k v -> print_newline (f1 k; print_string " "; f2 v)) m


let print_out o =
  print_newline
    (print_p_map
       (fun x -> print_int x; print_string " -")
       (List.iter (fun x -> print_int x; print_string " "))
       o)


let print_in i =
  print_p_map
    (fun x -> print_int x; print_string " -")
    (print_p_map print_int (fun x -> ()))
    i


let print_q q =
  print_newline (Queue.iter (fun x -> print_int x; print_string " ") q)


let zle = ref 0
let test n b =
  if not b then begin
    Printf.printf "Zly wynik testu %d!!
" n;
    incr zle
  end

let isOk input output =
	let rec where a = function
		| [] -> []
		| h::t -> if h = a then h::t else (where a t) in 
	let rec length used = function
		| [] -> List.length used
		| (h, l1)::t -> let newOne used a = 
					if (where a used) = [] then a::used else used
				in length (newOne (List.fold_left newOne used l1) h) t in
	let size = length [] input in
	let rec find acc wh = function
		| [] -> acc
		| h::t -> acc && ((where h wh) <> []) && (find acc wh t) in
	let rec pom acc = function
		| [] -> acc
		| (a, l)::t -> acc && find acc (where a output) l && pom acc t
	in (size = List.length output) && (pom true input);;

let genTest n =
	let res = ref [] in
	for i = 1 to (n-1) do
		if Random.int 10 < 6 then
		let l = ref [] and m = Random.int (n - i) in
		let used = Array.make (n+1) false in
		let unUsed _ =
			let getInt n = min n ((Random.int (max (n - i - 1) 1)) + i + 1) in
			let a = ref (getInt n) in
			while used.(!a) = true do
				if (!a) = n then a := (i + 1) else a := (!a) + 1;
			done; used.(!a) <- true; !a in
		for j = 0 to m do
			l := (unUsed j)::(!l)
		done;
		res := (i, !l)::(!res)
	done; (!res);;


(* Printf.printf "=== Podstawowe...
";; *)

let a = [];;
let b = [(1, [2;3]);(3, [6;7]);(6, [4;7]);(7, [5]);(2, [5]);(8, [])];;
let c = [(5, []);(2, [])];;
let d = [];;

test 1 (isOk a (topol a));;
test 2 (isOk b (topol b));;
test 3 (isOk c (topol c));;
test 4 (isOk d (topol d));;

(* Printf.printf "=== Inne typy...
";; *)

let a = [("a", ["c"]);("e", ["g"]);("f", ["a";"e"]);("g", ["c";"a"])];;
let b = [(false, [true])];;
let c = [("z", ["c"; "f"; "a"]);("f", ["x"; "a"]);("g", ["h"])];;
let d = [("xx", ["aa"; "gg"]);("ab", ["uw"; "mim"]);("mim", ["uw";"xx"])];;
let e = [("d", ["c"]);("c", ["b"]);("b", ["a"])];;

test 25 (isOk a (topol a));;
test 26 (isOk b (topol b));;
test 27 (isOk c (topol c));;
test 28 (isOk d (topol d));;
test 29 (isOk e (topol e));;


(* Printf.printf "=== Cykliczne..
";; *)

let a = [("a", ["b"]);("b", ["a"]);("c", ["a"])];;
let a = try topol a with 
	| Cykliczne -> [];;

let b = [("a", ["a"])];;
let b = try topol b with
	| Cykliczne -> [];;
List.iter print_string a;;

let c = [(1, [4; 5]);(3, [2]);(2, [3])];;
let c = try topol c with
	| Cykliczne -> [];;

let d = [(1, [2]);(2, [3; 4]);(3, [5; 6]);(6, [2])];;
let d = try topol d with
	| Cykliczne -> [];;

test 50 (a = []);;
test 51 (b = []);;
test 52 (c = []);;
test 53 (d = []);;

(* Printf.printf "=== Losowe..
";; *)

for i = 100 to 500 do
	let a = genTest 30 in
	test i (isOk a (topol a));
done;;

let _ = 
  if !zle <> 0 then Printf.printf "
Blednych testow: %d...
" !zle
;;
 *)
