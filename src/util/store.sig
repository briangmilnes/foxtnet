(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Brian Milnes (Brian.Milnes@cs.cmu.edu)
	Nick Haines  (Nick.Haines@cs.cmu.edu)
	Edoardo Biagioni <Edoardo.Biagioni@cs.cmu.edu>
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

		i.	Abstract

	store.sig: signature STORE and variations on the theme. A
	store is used to store and retrieve values based on keys,
	which must support the equality operator and a hash operation.
	Polymorphic (regular) stores have these operators supplied to
	the "new" function; monomorphic stores do not need them.
	Functional (regular) stores always return a new store;
	imperative stores update the store in place.  A single store
	is an imperative, monomorphic store where the store value
	itself is implicit.

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	signature STORE
	2.	signature MONOMORPHIC_STORE
	3.	signature IMPERATIVE_STORE
	4.	signature IMPERATIVE_MONOMORPHIC_STORE
	5.	signature SINGLE_STORE

		iii.	RCS Log
	
$Log: store.sig,v $
Revision 1.8  1997/02/13  00:36:17  esb
added several variants to the basic store type.

Revision 1.7  1996/04/18  21:35:06  cline
converted hash from int to word

Revision 1.6  1994/05/10  07:41:04  esb
added empty, equality predicate in new.

Revision 1.5  94/04/06  23:06:00  esb
added remove_selected.

Revision 1.4  94/01/17  17:54:12  esb
Standardized the interface.

Revision 1.3  1993/10/06  02:31:08  esb
major redesign.

Revision 1.2  1993/06/21  01:39:54  esb
changed (hopefully improved!) the interface to make it more flexible

Revision 1.1  1993/06/10  22:41:20  milnes
Initial revision


		1.	signature STORE
*)

signature STORE =
 sig
  type ('key, 'value) T		(* the type of the store itself *)

  (* create a new store with no elements. *)
  val new: ('key -> word) * ('key * 'key -> bool) -> ('key, 'value) T
  val size: ('key, 'value) T -> int
  val empty: ('key, 'value) T -> bool

  (* add a "key, element" pair *)
  val add: ('key, 'value) T * 'key * 'value -> ('key, 'value) T

  exception Not_Present_In_Store (* raised by find if the key is not present *)
  val find: ('key, 'value) T * 'key -> (('key, 'value) T * 'value)

  (* like find, but returns NONE if the key is not present.
   * if SOME is returned, we return a new store which may be optimized
   * for searching this value *)
  val look: ('key, 'value) T * 'key -> (('key, 'value) T * 'value) option

  (* no-op if key not present *)
  val remove: ('key, 'value) T * 'key -> ('key, 'value) T

  (* remove those elements for which the predicate returns true. *)
  val remove_selected: ('key, 'value) T * ('key * 'value -> bool)
                     -> ('key, 'value) T

  (* map and fold are defined by analogy with the list operations *)
  val map: ('key * 'a -> 'b) -> ('key, 'a) T -> ('key, 'b) T

  val fold: (('key * 'a) * 'b -> 'b) -> ('key, 'a) T -> 'b -> 'b

  val makestring: ('key, 'value) T * (('key * 'value) -> string)
                * string (* separator *) -> string

 end

(*
		2.	signature MONOMORPHIC_STORE
*)

signature MONOMORPHIC_STORE =
 sig
  type key
  type value
  type T		(* the type of the store *)

  (* create a new store with no elements. *)
  val new: unit -> T
  val size: T -> int
  val empty: T -> bool

  (* add a "key, element" pair *)
  val add: T * key * value -> T

  exception Not_Present_In_Store (* raised by find if the key is not present *)
  val find: T * key -> (T * value)

  (* like find, but returns NONE if the key is not present.
   * if SOME is returned, we return a new store which may be optimized
   * for searching this value *)
  val look: T * key -> (T * value) option

  (* no-op if key not present *)
  val remove: T * key -> T

  (* remove those elements for which the predicate returns true. *)
  val remove_selected: T * (key * value -> bool) -> T

  (* map, app, and fold are defined by analogy with the list operations *)
  val map: (key * value -> value) -> T -> T

  val app: (key * value -> 'a) -> T -> unit

  val fold: ((key * value) * 'a -> 'a) -> T -> 'a -> 'a

  val makestring: T * ((key * value) -> string) * string (* separator *)
                -> string

 end

(*
		3.	signature IMPERATIVE_STORE
*)

signature IMPERATIVE_STORE =
 sig
  type ('key, 'value) T		(* the type of the store itself *)

  (* create a new store with no elements. *)
  val new: ('key -> word) * ('key * 'key -> bool) -> ('key, 'value) T
  val size: ('key, 'value) T -> int
  val empty: ('key, 'value) T -> bool

  (* add a "key, element" pair *)
  val add: ('key, 'value) T * 'key * 'value -> unit

  exception Not_Present_In_Store (* raised by find if the key is not present *)
  val find: ('key, 'value) T * 'key -> 'value

  (* like find, but returns NONE if the key is not present.
   * if SOME is returned, we return a new store which may be optimized
   * for searching this value *)
  val look: ('key, 'value) T * 'key -> 'value option

  (* no-op if key not present *)
  val remove: ('key, 'value) T * 'key -> unit

  (* remove those elements for which the predicate returns true. *)
  val remove_selected: ('key, 'value) T * ('key * 'value -> bool) -> unit

  (* map and fold are defined by analogy with the list operations *)
  val map: ('key * 'a -> 'b) -> ('key, 'a) T -> ('key, 'b) T

  val fold: (('key * 'a) * 'b -> 'b) -> ('key, 'a) T -> 'b -> 'b

  val makestring: ('key, 'value) T * (('key * 'value) -> string)
                * string (* separator *) -> string

 end

(*
		4.	signature IMPERATIVE_MONOMORPHIC_STORE
*)

signature IMPERATIVE_MONOMORPHIC_STORE =
 sig
  type key
  type value
  type T		(* the type of the store itself *)

  (* create a new store with no elements. *)
  val new: unit -> T
  val size: T -> int
  val empty: T -> bool

  (* add a "key, element" pair *)
  val add: T * key * value -> unit

  exception Not_Present_In_Store (* raised by find if the key is not present *)
  val find: T * key -> value

  (* like find, but returns NONE if the key is not present.
   * if SOME is returned, we return a new store which may be optimized
   * for searching this value *)
  val look: T * key -> value option

  (* no-op if key not present *)
  val remove: T * key -> unit

  (* remove those elements for which the predicate returns true. *)
  val remove_selected: T * (key * value -> bool) -> unit

  (* map, app, and fold are defined by analogy with the list operations *)
  val map: (key * value -> value) -> T -> T

  val app: (key * value -> 'a) -> T -> unit

  val fold: ((key * value) * 'a -> 'a) -> T -> 'a -> 'a

  val makestring: T * ((key * value) -> string) * string (* separator *)
                -> string

 end

(*
		5.	signature SINGLE_STORE

	This is an imperative, monomorphic store that only provides
	access to a single store value for each structure.
*)

signature SINGLE_STORE =
 sig
  type key
  type value

  val size: unit -> int
  val empty: unit -> bool

  (* add a "key, element" pair *)
  val add: key * value -> unit

  exception Not_Present_In_Store (* raised by find if the key is not present *)
  val find: key -> value

  (* like find, but returns NONE if the key is not present.
   * if SOME is returned, we return a new store which may be optimized
   * for searching this value *)
  val look: key -> value option

  (* no-op if key not present *)
  val remove: key -> unit

  (* remove those elements for which the predicate returns true. *)
  val remove_selected: (key * value -> bool) -> unit

  (* map and fold are defined by analogy with the list operations *)
  val app: (key * value -> 'a) -> unit

  val fold: ((key * value) * 'a -> 'a) -> 'a -> 'a

  val makestring: ((key * value) -> string) * string (* separator *) -> string

 end
