(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edoardo Biagioni <Edoardo.Biagioni@cs.cmu.edu>
	Brian Milnes (Brian.Milnes@cs.cmu.edu)
	Ken Cline (Kenneth.Cline@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

		i.	Abstract

	A signature for dynamic arrays which can be shortened from
	either the front or the back.


		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	signature DYNAMIC_BYTE_ARRAY
	2.	creation functions
	3.	conversion functions
	4.	aggregate functions
	5.	element functions
	6.	element functions

		iii.	RCS Log
	
$Log: dynarray.sig,v $
Revision 1.4  1995/03/24  01:48:50  esb
changed interface to makestring.

Revision 1.3  1995/02/13  23:00:05  esb
redesigned map, added app and the string conversion functions.

Revision 1.2  1994/09/30  16:25:52  esb
changed to work with SML/NJ 1.05 as well as 0.93.

Revision 1.1  1994/08/02  20:17:30  esb
Initial revision


		1.	signature DYNAMIC_BYTE_ARRAY
*)

signature DYNAMIC_BYTE_ARRAY =
 sig
  type T

  (* raised if an access would violate the bounds of the array. *)
  exception Subscript
  (* raised if a ubyteN operation (N > 1) is applied and the
     size of the array are not multiples of N. *)
  exception Alignment
  (* raised on a tail operation if the number of elements to be
     hidden is greater than the number of elements in the array. *)
  exception Size

(*
		2.	creation functions
*)

  val new: int -> T			(* zero-filled *)

  val init: ByteArray.bytearray -> T

  val init1: int * (int -> FoxWord8.word) -> T
  val init2: int * (int -> FoxWord16.word) -> T    (* may raise Alignment. *)
  val init4: int * (int -> FoxWord32.word) -> T    (* may raise Alignment. *)

  val init_list1: FoxWord8.word list -> T
  val init_list2: FoxWord16.word list -> T
  val init_list4: FoxWord32.word list -> T

  val init_string: string -> T

(*
		3.	conversion functions

	The array function returns a byte array which shares with
	the dynamic array.  If the array is a result of a call to
	tail, revtail, or append, this cannot be done and we raise
	Illegal_Array.  All other functions work on all dynamic arrays.
*)

  exception Illegal_Array

  val size: T -> int

  val array: T -> ByteArray.bytearray	(* may raise Illegal_Array *)

  val read: T -> ByteArray.bytearray

  val to_list1: T -> FoxWord8.word list
  val to_list2: T -> FoxWord16.word list	(* may raise Alignment. *)
  val to_list4: T -> FoxWord32.word list	(* may raise Alignment. *)

  val makestring: {data: T, start_print: int, max_length: int, base: int,
		   separator: string} -> string

(*
		4.	aggregate functions

	Copy copies bytes from one array to another. Append merges
	the arrays to produce a new array which shares with the
        given ones; calling "array" on the result of such a call
	will raise Illegal_Array.
*)

  val copy: T * int * int * T * int -> unit (* may raise Subscript. *)

  val append: T list -> T

  val equal: T * T -> bool

  val checksum: T * int * int -> FoxWord16.word	(* may raise Subscript *)

  (* make the first n elements inaccessible, decrease all remaining
     indices by n.  The resulting array shares with the original array. *)
  val tail: T * int -> T			(* may raise Size *)

  (* make the last n elements inaccessible.  The resulting array shares
     with the original array. *)
  val revtail: T * int -> T			(* may raise Size *)

(*
		5.	element functions
*)

  val sub: T * int * int -> ByteArray.bytearray	(* may raise Subscript. *)
  val sub1: T * int -> FoxWord8.word     (* may raise Subscript. *)
  val sub2: T * int -> FoxWord16.word    (* may raise Subscript. *)
  val sub4: T * int -> FoxWord32.word    (* may raise Subscript. *)
  val substring: T * int * int -> string (* may raise Subscript. *)

  val update: T * int * ByteArray.bytearray -> unit (* may raise Subscript. *)
  val update1: T * int * FoxWord8.word -> unit      (* may raise Subscript. *)
  val update2: T * int * FoxWord16.word -> unit     (* may raise Subscript. *)
  val update4: T * int * FoxWord32.word -> unit     (* may raise Subscript. *)
  val updatestring: T * int * string -> unit        (* may raise Subscript. *)

  (* map2 and map4 may raise Alignment *)
  val map1: (int * FoxWord8.word -> FoxWord8.word) -> T -> T
  val map2: (int * FoxWord16.word -> FoxWord16.word) -> T -> T
  val map4: (int * FoxWord32.word -> FoxWord32.word) -> T -> T

  (* app2 and app4 may raise Alignment *)
  val app1: T * (int * FoxWord8.word -> FoxWord8.word) -> unit
  val app2: T * (int * FoxWord16.word -> FoxWord16.word) -> unit
  val app4: T * (int * FoxWord32.word -> FoxWord32.word) -> unit

  (* fold2 and fold4 may raise Alignment *)
  val fold1: (int * FoxWord8.word * 'b -> 'b) -> T -> 'b -> 'b
  val fold2: (int * FoxWord16.word * 'b -> 'b) -> T -> 'b -> 'b
  val fold4: (int * FoxWord32.word * 'b -> 'b) -> T -> 'b -> 'b

(*
		6.	zero-sized array
*)

  val empty: T

 end
