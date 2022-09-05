(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edoardo Biagioni (Edoardo.Biagioni@cs.cmu.edu)
	Ken Cline (Kenneth.Cline@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15213-3891

		i.	Abstract

	monoseq.sig: signatures for monomorphic sequence type.

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	signature SEQ
	2.	signature CONVERT_SEQ
	3.	signature FINITE_SEQ
	4.	signature COMPARE_SEQ
	5.	signature LIST_SEQ
	6.	signature STREAM_SEQ
	7.	signature CHANNEL_SEQ
	8.	signature ARRAY_SEQ
	9.	signature ENDIAN_ARRAY
	10.	signature WORD_ARRAY
	11.	signature EXPOSE_WORD_ARRAY

		iii.	RCS Log
	
$Log: monoseq.sig,v $
Revision 1.6  1995/09/18  19:32:37  esb
added signature EXPOSE_WORD_ARRAY.

Revision 1.5  1995/07/21  12:53:25  esb
added sharing for T, element in ENDIAN_ARRAY.

Revision 1.4  1995/07/05  17:47:43  esb
changed the definition of new to eliminate generator and iterator.

Revision 1.3  1995/06/29  18:23:01  esb
restructured.

Revision 1.2  1995/06/21  20:20:05  esb
added tabulate.

Revision 1.1  1995/06/20  17:32:56  esb
Initial revision


		1.	signature SEQ

	Monomorphic sequences.
*)

signature SEQ =
 sig
  type element
  type T

  val new: ('b -> (element * 'b) option) -> 'b -> T

  val next: T -> (element * T) option
  val seek: T * int -> T

  exception Empty
  val head: T -> element		(* may raise Empty *)
  val tail: T -> T			(* may raise Empty *)
  val nth: T * int -> element		(* may raise Empty *)

  val isempty: T -> bool		(* O(1) *)
  val wait: T -> unit		(* O(n), may not terminate *)

  val map: (element -> element) -> T -> T
  val app: (element -> unit) -> T -> unit
  val fold: (element * 'a -> 'a) -> 'a -> T -> 'a
  val filter: (element -> element option) -> T -> T
 end

(*
		2.	signature CONVERT_SEQ

	A generic conversion module from any one sequence to any
	other sequence.  If both structures are lazy/nonterminating,
	the implementation will preserve laziness/nontermination.
*)

signature CONVERT_SEQ =
 sig
  structure From: SEQ
  structure To: SEQ
   sharing type From.element = To.element

  val convert: From.T -> To.T
 end

(*
		3.	signature FINITE_SEQ

	As well as reverse forms of the various functions for iterating
	on the head of the sequence, finite sequences offer reverse,
	append, length, and comparison functions.
*)

signature FINITE_SEQ =
 sig
  include SEQ

  val create: element * int -> T
  val tabulate: int * (int -> element) -> T

  val reverse: T -> T
  val append: T * T -> T

  val length: T -> int
  val equal: T * T -> bool
  val less: T * T -> bool

  structure Rev: SEQ
   sharing type Rev.T = T
       and type Rev.element = element
 end

(*
		4.	signature COMPARE_SEQ

	Gives a description of the differences between two finite sequences.
*)

signature COMPARE_SEQ =
 sig
  type T
  val compare: T * T -> string
 end

(*
		5.	signature LIST_SEQ
*)

signature LIST_SEQ =
 sig
  include FINITE_SEQ

  datatype list = nil | :: of element * list
  sharing type T = list
 end

(*
		6.	signature STREAM_SEQ

	"Purely functional" streams (most implementations will be
	memoized).
*)

signature STREAM_SEQ =
 sig
  include SEQ

  val empty: T
  val cons: element * T -> T
  val lazy_cons: element * (unit -> T) -> T
  val delay: (unit -> T) -> T
 end

(*
		7.	signature CHANNEL_SEQ

	Imperative channels.
*)

signature CHANNEL_SEQ =
 sig
  include SEQ

  val create: unit -> T
  val enqueue: element * T -> unit
  val close: T -> unit
  val size: T -> int
  val flush: T -> T
 end

(*
		8.	signature ARRAY_SEQ

	Updatable fixed-size sequences.
*)

signature ARRAY_SEQ =
 sig
  include FINITE_SEQ

  type updatable

  val write: T -> updatable option
  val rev_write: T -> updatable option
  val read: updatable -> T
  val update: updatable * element -> updatable option
 end (* sig *)

(*
		9.	signature ENDIAN_ARRAY

	Arrays with big- and little-endian operations.
*)

signature ENDIAN_ARRAY =
 sig
  include ARRAY_SEQ

  structure Big: ARRAY_SEQ
  structure Little: ARRAY_SEQ
    sharing type T = Big.T = Little.T
        and type element = Big.element = Little.element

  val native_big_endian: bool

 end (* sig *)

(*
		10.	signature WORD_ARRAY

	Conversion of n-bit-arrays to m-bit-arrays where m > n may
	"lose" any final odd n-bit words.  These are returned
	as part of the conversion and also restored upon inverse
	conversion.  Unaligned is raised if the start of the array
	is at an odd position.
*)

signature WORD_ARRAY =
 sig
  type T
  structure W8 : ENDIAN_ARRAY
  structure W16: ENDIAN_ARRAY
  structure W32: ENDIAN_ARRAY
  structure W64: ENDIAN_ARRAY
   sharing type W8.element  = FoxWord8.word
       and type W16.element = FoxWord16.word
       and type W32.element = FoxWord32.word
       and type W64.element = FoxWord64.word
       and type T = W8.T

  exception Unaligned			(* raised by convert16/32/64 *)

  datatype source = Array8  of W8.T
                  | Array16 of W16.T
                  | Array32 of W32.T
                  | Array64 of W64.T

  val convert8 : source -> W8.T
  val convert16: source -> (W16.T * W8.T)
  val convert32: source -> (W32.T * W8.T)
  val convert64: source -> (W64.T * W8.T)

 end (* sig *)

(*
		11.	signature EXPOSE_WORD_ARRAY

		This is like a WORD_ARRAY, but provides a handle to
		the actual physical underlying array.  This is
		used mostly for raw I/O.
*)

signature EXPOSE_WORD_ARRAY =
 sig
  include WORD_ARRAY

  val expose: T -> ByteArray.bytearray * int (* first *) * int (* last *)
 end


