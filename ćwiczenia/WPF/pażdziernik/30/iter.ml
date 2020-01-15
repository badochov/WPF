(* iter ogonowo *)
let rec iter_t n f x = if n = 0 then x else iter_t (n - 1) f (f x)

(* iter nieogonowo *)
let rec iter_nt n f x = if n = 0 then x else f (iter_nt (n - 1) f x)

(* łączenie funkcji *)
let compose f g x = f (g x)

(* funkcja identycznościowa *)
let id x = x

(* * to  złożenie funkcji *)

(* iter bez argumentu x *)
(* f * (f * ... * (f * id) ... ) *)
let rec iter_nx n f = if n = 0 then id else compose f (iter_nx (n - 1) f)

(* iter bez  argumentu x v2 *)
(* ( ... (id * f) * f ... ) * f *)
let rec iter_nx2 n f = if n = 0 then id else compose (iter_nx2 (n - 1) f) f

(* funkcja czekająca na argument *)
let h1 = iter_t 17 succ

(* funkcja czekająca na argument *)
let h2 = iter_nt 17 succ

(* funkcja  typu x | f (g x), gdzie f to succ, a g to strzałako do następnego compose, a na końcu id
    czekająca na argument *)
let h3 = iter_nx 17 succ

(* funkcja  typu x | f (g x), gdzie f to strzałka do następnego compose, a na końcu id, a g to succ
    czekająca na argument *)
let h4 = iter_nx 17 succ
