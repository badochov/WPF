(* 
    Dana jest prostokątna mapa wysokości górzystego terenu, w postaci prostokątnej tablicy dodatnich liczb całkowitych, o wymiarachN×M.
    Chcemy przejść z pola o współrzędnych(0,0) na pole o współrzędnych(N−1,M−1), ale nie chcemy wspinać się zbyt wysoko.
    (Możemy się przesuwać w kierunkach N, W, S, E.)
    Napisz proceduręwysokosc:int array array→int, która dla danej mapy terenu określi
    minimalną największą wysokość, na którą musimy wejść w trakcie podróży. 
*)

(** Tymczasowy moduł kolejki priorytetowej *)
module type PQueue = sig
  type 'a t

  type key = int

  val empty : 'a t

  val is_empty : 'a t -> bool

  exception Empty

  val add : key -> 'a -> 'a t -> 'a t

  val remove_min : 'a t -> 'a * 'a t
end

module PQ : PQueue = struct
  type 'a t = 'a list

  type key = int

  let empty = []

  let is_empty q = false

  exception Empty

  let add k el q = q

  let remove_min q = (List.hd q, q)
end

let sasiedzi = [(-1, 0); (1, 0); (0, 1); (0, -1)]

let wysokosc mapa =
  let n = Array.length mapa in
  let m = Array.length mapa.(0) in
  let q = ref PQ.empty in
    q := PQ.add mapa.(0).(0) (0, 0) !q;
    let koniec = ref false in
    let poziom = ref 0 in
    let odw = Array.make_matrix n m false in
      odw.(0).(0) <- true;
      while (not !koniec) && not (PQ.is_empty !q) do
        let ((x, y), nq) = PQ.remove_min !q in
          q := nq;
          poziom := max !poziom mapa.(x).(y);
          if x = n - 1 && y = m - 1
          then koniec := true
          else
            List.iter
              (fun (dx, dy) ->
                let nx = x + dx in
                let ny = y + dy in
                  if nx >= 0 && nx < n && nx >= 0 && nx < m
                     && not odw.(nx).(ny)
                  then q := PQ.add mapa.(nx).(ny) (nx, ny) !q;
                  odw.(nx).(ny) <- true)
              sasiedzi
      done;
      !poziom


let () =
  assert (wysokosc [|[|1|]|] = 1);
  assert (wysokosc [|[|1; 2|]|] = 2);
  assert (wysokosc [|[|1; 2|]; [|3; 3|]|] = 3);
  assert (wysokosc [|[|1; 2|]; [|3; 4|]|] = 4)
