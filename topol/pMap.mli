(*
 * PMap - Polymorphic maps
 * Copyright (C) 1996-2003 Xavier Leroy, Nicolas Cannasse, Markus Mottl
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version,
 * with the special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

(** Polymorphic Map.

	This is a polymorphic map, similar to standard library [Map] module
	but in a defunctorized style.
*)

type ('a, 'b) t

(** The empty map, using [compare] as key comparison function. *)
val empty : ('a, 'b) t

(** returns true if the map is empty. *)
val is_empty : ('a, 'b) t -> bool

(** creates a new empty map, using the provided function for key comparison.*)
val create : ('a -> 'a -> int) -> ('a, 'b) t

(** [add x y m] returns a map containing the same bindings as
    [m], plus a binding of [x] to [y]. If [x] was already bound
    in [m], its previous binding disappears. *)
val add : 'a -> 'b -> ('a, 'b) t -> ('a, 'b) t

(** [find x m] returns the current binding of [x] in [m],
    or raises [Not_found] if no such binding exists. *)
val find : 'a -> ('a, 'b) t -> 'b

(** [remove x m] returns a map containing the same bindings as
    [m], except for [x] which is unbound in the returned map. *)
val remove : 'a -> ('a, 'b) t -> ('a, 'b) t

(** [mem x m] returns [true] if [m] contains a binding for [x],
    and [false] otherwise. *)
val mem : 'a -> ('a, 'b) t -> bool

(** same as [mem]. *)
val exists : 'a -> ('a, 'b) t -> bool

(** [iter f m] applies [f] to all bindings in map [m].
    [f] receives the key as first argument, and the associated value
    as second argument. The order in which the bindings are passed to
    [f] is unspecified. Only current bindings are presented to [f]:
    bindings hidden by more recent bindings are not passed to [f]. *)
val iter : ('a -> 'b -> unit) -> ('a, 'b) t -> unit

(** [map f m] returns a map with same domain as [m], where the
    associated value [a] of all bindings of [m] has been
    replaced by the result of the application of [f] to [a].
    The order in which the associated values are passed to [f]
    is unspecified. *)
val map : ('b -> 'c) -> ('a, 'b) t -> ('a, 'c) t

(** Same as [map], but the function receives as arguments both the
    key and the associated value for each binding of the map. *)
val mapi : ('a -> 'b -> 'c) -> ('a, 'b) t -> ('a, 'c) t

(** [fold f m a] computes [(f kN dN ... (f k1 d1 a)...)],
    where [k1 ... kN] are the keys of all bindings in [m],
    and [d1 ... dN] are the associated data.
    The order in which the bindings are presented to [f] is
    unspecified. *)
val fold : ('b -> 'c -> 'c) -> ('a, 'b) t -> 'c -> 'c

(** Same as [fold], but the function receives as arguments both the
    key and the associated value for each binding of the map. *)
val foldi : ('a -> 'b -> 'c -> 'c) -> ('a, 'b) t -> 'c -> 'c
