(* Author: Hubert Badocha, based on work PSet - Polimorphic Sets of Xavier
   Leroy, Nicolas Cannasse, Markus Mottl, Jacek Chrząszcz provided by
   University of Warsaw *)
(* Code review: Grzegorz Szumigaj *)

(** Represents interval from first to second element Assumes first element is
    smaller than second *)

type interval = int * int

(** Represents tree It is in form of Either Leaf for leaves of the tree or Node
    for nodes which are tuples in form ( left child, value, right child,
    height, tuple where first element is #integers in left child and second in
    right child ) *)
type t =
  | Node of t * interval * t * int * (int * int)
  | Leaf

(** Used for checking overflow during addition If a+b < return max_int
    otherwise a+b *)
let ( +$ ) a b =
  let sum = a + b in
    if sum < 0 then max_int else sum


(** returns height of given tree *)
let height = function Node (_, _, _, h, _) -> h | Leaf -> 0

(** Returns the size of given interval *)
let intvl_size (min, max) =
  let diff = max - min in
    if diff >= 0 then diff +$ 1 else max_int


(** Returns # of numbers in given tree *)
let ints_in_tree = function
  | Leaf ->
      0
  | Node (l, k, _, _, (ints_in_l, ints_in_r)) ->
      ints_in_l +$ ints_in_r +$ intvl_size k


(** creates tree *)
let make l k r =
  let h = max (height l) (height r) + 1
  and numbers_below = (ints_in_tree l, ints_in_tree r) in
    Node (l, k, r, h, numbers_below)


(** Type of outcomes of interval comparison *)
type comparison_outcome =
  | Bigger
  | Smaller
  | Overlapping

(** cmp i1 i2 returns Bigger -> i1 < i2 Smaller -> i1 > i2 Overlapping self
    explainatory *)
let cmp (base_min, base_max) (var_min, var_max) =
  if var_min > base_max
  then Bigger
  else if base_min > var_max
  then Smaller
  else Overlapping


(** Raised when trees passed to bal don't meet requirements *)
exception BalanceFailure

(** creates balanced tree takes tree interval and tree *)
let bal l intvl r =
  let hl = height l in
  let hr = height r in
    if hl > hr + 2
    then
      match l with
      | Node (ll, lk, lr, _, _) -> (
          if height ll >= height lr
          then make ll lk (make lr intvl r)
          else
            match lr with
            | Node (lrl, lrk, lrr, _, _) ->
                make (make ll lk lrl) lrk (make lrr intvl r)
            | Leaf ->
                raise BalanceFailure )
      | Leaf ->
          raise BalanceFailure
    else if hr > hl + 2
    then
      match r with
      | Node (rl, rk, rr, _, _) -> (
          if height rr >= height rl
          then make (make l intvl rl) rk rr
          else
            match rl with
            | Node (rll, rlk, rlr, _, _) ->
                make (make l intvl rll) rlk (make rlr rk rr)
            | Leaf ->
                raise BalanceFailure )
      | Leaf ->
          raise BalanceFailure
    else make l intvl r


(** Returns min element from the tree *)
let rec min_elt = function
  | Node (Leaf, k, _, _, _) ->
      k
  | Node (l, _, _, _, _) ->
      min_elt l
  | Leaf ->
      raise Not_found


(** return tree after removal of min element *)
let rec remove_min_elt = function
  | Node (Leaf, _, r, _, _) ->
      r
  | Node (l, k, r, _, _) ->
      bal (remove_min_elt l) k r
  | Leaf ->
      invalid_arg "PSet.remove_min_elt"


(** Merges t1 t2 Assumes t1 and t2 are balanced and every element in t2 is
    bigger than in t1 *)
let merge t1 t2 =
  match (t1, t2) with
  | (Leaf, _) ->
      t2
  | (_, Leaf) ->
      t1
  | _ ->
      let k = min_elt t2 in
        bal t1 k (remove_min_elt t2)


(** The empty set *)
let empty = Leaf

(** returns true if the set is empty. *)
let is_empty x = x = Leaf

(** Raised when add_one is executed with tree not meeting the requirements *)
exception UnpreparedTree

(** Helper for add function takes comparator, interval and tree returns tree
    after insertion of new element Assumes interval will not overlap with any
    element in the tree Throws UnpreparedTree otherwise *)
let rec add_one intvl = function
  | Node (l, k, r, h, _) -> (
      match cmp k intvl with
      | Overlapping ->
          raise UnpreparedTree
      | Smaller ->
          let nl = add_one intvl l in
            bal nl k r
      | Bigger ->
          let nr = add_one intvl r in
            bal l k nr )
  | Leaf ->
      make Leaf intvl Leaf


(** Joins left,right tree and interval into balanced tree Assumes both are
    balanced and every element in left is smaller than interval and every in
    right bigger than interval *)
let rec join l intvl r =
  match (l, r) with
  | (Leaf, _) ->
      add_one intvl r
  | (_, Leaf) ->
      add_one intvl l
  | (Node (ll, lv, lr, lh, _), Node (rl, rv, rr, rh, _)) ->
      if lh > rh + 2
      then bal ll lv (join lr intvl r)
      else if rh > lh + 2
      then bal (join l intvl rl) rv rr
      else make l intvl r


(** Splits interval by given number then adds those parts to correspoing tree
    with either bigger or smaller elements Takes interval, number to split it
    with, left tree -> with all elements smaller than num right tree -> with
    all elements greater than num

    Return left and right tree after merge with interval split by number *)
let merge_intvl_into_children (min_val, max_val) num left_tree right_tree =
  let l =
    if min_val < num
    then add_one (min_val, min max_val (num - 1)) left_tree
    else left_tree
  and r =
    if max_val > num
    then add_one (max min_val (num + 1), max_val) right_tree
    else right_tree
  in
    (l, r)


(** [split x s] returns a triple [(l, present, r)], where [l] is the set of
    elements of [s] that are strictly lesser than [x]; [r] is the set of
    elements of [s] that are strictly greater than [x]; [present] is [false] if
    [s] contains no element equal to [x], or [true] if [s] contains an element
    equal to [x]. *)
let split num set =
  let rec loop = function
    | Leaf ->
        (Leaf, false, Leaf)
    | Node (l, k, r, _, _) ->
      match cmp k (num, num) with
      | Overlapping ->
          let (new_l, new_r) = merge_intvl_into_children k num l r in
            (new_l, true, new_r)
      | Smaller ->
          let (ll, pres, rl) = loop l in
            (ll, pres, join rl k r)
      | Bigger ->
          let (lr, pres, rr) = loop r in
            (join l k lr, pres, rr)
  in
    loop set


(** [remove (x, y) s] returns a set containing the same elements as [s], except
    for all those which are included between [x] and [y]. Assumes [x <= y]. *)
let remove (min_val, max_val) set =
  let (l, _, temp_r) = split min_val set in
  let (_, _, r) = split max_val temp_r in
    match r with Leaf -> l | Node _ -> join l (min_elt r) (remove_min_elt r)


(** Decrease number by 1 or min_int if number given is min_int *)
let decrease num = if num = min_int then min_int else num - 1

(** Increase number by 1 or max_int if number given is max_int *)
let increase num = if num = max_int then max_int else num + 1

(** Finds max value of interval after merging it with all possible to merge
    with intervals in the tree *)
let rec max_intvl_val_after_merge ((min_val, max_val) as intvl) = function
  | Node (l, k, r, _, _) -> (
      match cmp (decrease min_val, increase max_val) k with
      | Bigger ->
          max_intvl_val_after_merge intvl l
      | Smaller ->
          max_intvl_val_after_merge intvl r
      | Overlapping ->
          if snd intvl <= snd k
          then snd k
          else max_intvl_val_after_merge intvl r )
  | Leaf ->
      snd intvl


(** Finds min value of interval after merging it with all possible to merge
    with intervals in the tree *)
let rec min_intvl_val_after_merge ((min_val, max_val) as intvl) = function
  | Node (l, k, r, _, _) -> (
      match cmp (decrease min_val, increase max_val) k with
      | Bigger ->
          min_intvl_val_after_merge intvl l
      | Smaller ->
          min_intvl_val_after_merge intvl r
      | Overlapping ->
          if fst intvl >= fst k
          then fst k
          else min_intvl_val_after_merge intvl l )
  | Leaf ->
      fst intvl


(** Returns interval after merging it with every possible interval in the set *)
let merge_with_overlapping intvl set =
  let min_val = min_intvl_val_after_merge intvl set
  and max_val = max_intvl_val_after_merge intvl set in
    (min_val, max_val)


(** [add (x, y) s] returns a set containing the same elements as [s], plus all
    elements of the interval [[x,y]] including [x] and [y]. Assumes [x <= y]. *)
let add intvl set =
  let merged_intvl = merge_with_overlapping intvl set in
  let set_after_removal = remove merged_intvl set in
    add_one merged_intvl set_after_removal


(** Helper function for below Returns number of elements smaller than given
    number Takes number accumulator and tree *)
let rec below_helper num acc = function
  | Leaf ->
      acc
  | Node (l, (min_val, max_val), r, _, (ints_in_l, _)) ->
      if min_val > num
      then below_helper num acc l
      else if max_val >= num
      then acc +$ intvl_size (min_val, min num max_val) +$ ints_in_l
      else
        let acc = acc + (ints_in_l +$ intvl_size (min_val, max_val)) in
          if acc < 0 then max_int else below_helper num acc r


(** [below n s] returns the number of elements of [s] that are lesser or equal
    to [n]. If there are more than max_int such elements, the result should be
    max_int. *)
let below num set = below_helper num 0 set

(** [mem x s] returns [true] if [s] contains [x], and [false] otherwise. *)
let mem x set =
  let rec loop = function
    | Node (l, k, r, _, _) -> (
        match cmp k (x, x) with
        | Overlapping ->
            true
        | Smaller ->
            loop l
        | Bigger ->
            loop r )
    | Leaf ->
        false
  in
    loop set


(** [iter f s] applies [f] to all continuous intervals in the set [s]. The
    intervals are passed to [f] in increasing order. *)
let iter f set =
  let rec loop = function
    | Leaf ->
        ()
    | Node (l, k, r, _, _) ->
        loop l; f k; loop r
  in
    loop set


(** [fold f s a] computes [(f xN ... (f x2 (f x1 a))...)], where x1 ... xN are
    all continuous intervals of s, in increasing order. *)
let fold f set acc =
  let rec loop acc = function
    | Leaf ->
        acc
    | Node (l, k, r, _, _) ->
        loop (f k (loop acc l)) r
  in
    loop acc set


(** Return the list of all continuous intervals of the given set. The returned
    list is sorted in increasing order. *)
let elements set =
  let rec loop acc = function
    | Leaf ->
        acc
    | Node (l, k, r, _, _) ->
        loop (k :: loop acc r) l
  in
    loop [] set

(* Tests *)
(* #use "iSet.ml" let good = ref 0 and bad = ref 0;;

   let check nr warunek wartosc = if warunek = wartosc then begin (*
   Printf.printf "OK - TEST nr %d " nr; *) incr good end else begin
   Printf.printf "Fail: %d " nr; assert (false); end;;

   let liczba a = List.length (elements a)

   (* Testy na add i remove *)

   let a = empty let a = add (17, 20) a let a = add (5, 8) a let a = add (1, 2)
   a let a = add (10, 12) a let a = add (28, 35) a let a = add (22, 23) a let a
   = add (40, 43) a let a = add (37, 37) a;;

   check 1 (is_empty a) false;; check 2 (mem 29 a) true;; check 3 (mem 21 a)
   false;; check 4 (mem 38 a) false;; check 5 (mem 37 a) true;; check 6 (below
   8 a = below 9 a) true;; check 7 (below 29 a) 17;; check 8 (liczba a) 8;;

   let a = add (37, 42) a;;

   check 9 (liczba a) 7;; check 10 (mem 37 a) true;; check 11 (mem 38 a) true;;
   check 12 (mem 39 a) true;; check 13 (mem 40 a) true;; check 14 (mem 41 a)
   true;; check 15 (mem 42 a) true;; check 16 (mem 44 a) false;; check 17
   (below 38 a = below 39 a) false;;

   let tmp = remove (8, 22) a;; let tmp = add (8, 22) tmp;;

   check 18 (elements tmp = elements a) false;;

   (* Testy na split *)

   let (l, exists, p) = split 9 a;;

   check 19 exists false;; check 20 (liczba l) 2;; check 21 (liczba p) 5;;
   check 22 (mem 10 l) false;; check 23 (mem 9 l) false;; check 24 (mem 8 l)
   true;; check 25 (mem 1 l) true;; check 26 (mem 9 p) false;; check 27 (mem 10
   p) true;; check 28 (mem 17 p) true;; check 29 (mem 29 p) true;; check 30
   (mem 24 p) false;; check 31 (mem 38 p) true;; check 32 ((elements l @
   elements p) = elements a) true;;

   let (l, exists, p) = split 21 a;;

   check 33 exists false;; check 34 ((elements l @ elements p) = elements a)
   true;;

   let (l, exists, p) = split 15 a;; check 35 exists false;; check 36
   ((elements l @ elements p) = elements a) true;;

   let b = empty let b = add (5, 10) b let b = add (40, 50) b let b = add (20,
   25) b let b = add (12, 14) b let b = add (17, 18) b let b = add (52, 60) b
   let b = add (62, 80) b let b = add (83, 100) b;;

   check 37 (mem 41 b) true;; check 38 (mem 11 b) false;;

   let d = empty;; let (l, ex, p) = split 0 d;;

   check 39 (is_empty l) true;; check 40 (is_empty p) true;;

   let d = add (17, 30) d;; let d = add (1, 3) d;; let d = add (10, 10) d;; let
   d = remove (11, 11) d;; let d = add (12, 14) d;; let d = add (32, 35) d;;
   let d = add (38, 40) d;;

   check 41 (below 36 d = below 37 d) true;;

   let d = add (36, 37) d;;

   check 42 (below 36 d = below 37 d) false;;

   let d = remove (37, 37) d;; check 43 (below 36 d = below 37 d) true;;

   let d = remove (20, 21) d;;

   check 44 (elements d) [(1, 3); (10, 10); (12, 14); (17, 19); (22, 30); (32,
   36); (38, 40)];;

   let (l, ex, p) = split 15 d;; check 144 (elements l) [(1, 3); (10, 10); (12,
   14)];; check 145 (elements p) [(17, 19); (22, 30); (32, 36); (38, 40)];;

   check 45 ((elements l @ elements p) = elements d) true;; check 46 (liczba l,
   liczba p) (3, 4);;

   check 47 (mem 13 l) true;; check 48 (mem 14 l) true;; check 49 ex false;;

   let (l, ex, p) = split 25 d;;

   check 50 ex true;; check 51 (elements l) [(1, 3); (10, 10); (12, 14); (17,
   19); (22, 24)];; check 52 (elements p) [(26, 30); (32, 36); (38, 40)];; *)
