(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edoardo Biagioni (Edoardo.Biagioni@cs.cmu.edu)
	Ken Cline (Kenneth.Cline@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15213-3891

		i.	Abstract

	seq.sig: signatures for monomorphic sequence type.

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
	9.	signature REV_ARRAY_SEQ
	10.	signature BYTE_ACCESS_ARRAY
	11.	signature WORD_ARRAY
	12.	signature EXPOSE_WORD_ARRAY

		iii.	RCS Log
	
$Log: seq.sig,v $
Revision 1.10  1997/11/14  11:55:44  cline
changed rigid sharing constraing to where type

Revision 1.9  96/04/25  20:35:06  esb
eliminated W1, W2, and W4 from WORD_ARRAY.

Revision 1.8  1996/03/11  20:06:57  cline
updated for 109.6

Revision 1.7  1996/02/06  23:42:09  esb
tabulate takes a word size instead of an integer size.

Revision 1.6  1996/01/19  23:07:00  esb
changed several ints to words, since they could not be negative.

Revision 1.5  1996/01/15  18:00:49  cline
removed FoxWord

Revision 1.4  1995/11/12  16:43:53  esb
this file now has monomorphic signatures; polymorphic ones
are now in seq.sig.


		1.	signature SEQ

	Monomorphic sequences.
*)

signature SEQ =
 sig
  type element
  type T

  val new: ('b -> (element * 'b) option) -> 'b -> T

  val next: T -> (element * T) option
  val seek: T * Word.word -> T

  exception Empty
  val head: T -> element		(* may raise Empty *)
  val tail: T -> T			(* may raise Empty *)
  val nth: T * Word.word -> element		(* may raise Empty *)

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

  val create: element * Word.word -> T
  val tabulate: (Word.word -> element) * Word.word -> T

  val equal: T * T -> bool
  val less: T * T -> bool

  val length: T -> Word.word
  val append: T * T -> T
  val reverse: T -> T

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
  val create_uninitialized: Word.word -> T

  type U

  val write: T -> U option
  val update: U * element -> U option
  val read: U -> T
 end (* sig *)

(*
		9.	signature REV_ARRAY_SEQ

	Updatable fixed-size sequences accessible at both ends.
*)

signature REV_ARRAY_SEQ =
 sig
  type T
  type element
  type U

  structure F: ARRAY_SEQ
  structure R: ARRAY_SEQ
   sharing type T = R.T = F.T
       and type U = R.U = F.U
       and type element = R.element = F.element
 end (* sig *)

(*
		10.	signature BYTE_ACCESS_ARRAY

	Arrays with big- and little-endian operations, as well as
	native operations.  Native operations are as fast as possible
	and the underlying array is always aligned and it contains a
	words with no extra bytes.

	An endian array also provides operations for:
	- arrays that are aligned at the front, that is, for which
          fast access from the front is supported
	- arrays that are aligned at the back, that is, for which
          fast access from the back is supported
	- arrays that are unaligned
	- functions to convert between these different array types

*)

signature BYTE_ACCESS_ARRAY =
 sig
  structure Native: REV_ARRAY_SEQ
  structure Big: REV_ARRAY_SEQ
  structure Little: REV_ARRAY_SEQ

  structure F_Big: ARRAY_SEQ
  structure F_Little: ARRAY_SEQ
  structure R_Big: ARRAY_SEQ
  structure R_Little: ARRAY_SEQ

  structure U_Big: REV_ARRAY_SEQ
  structure U_Little: REV_ARRAY_SEQ

  type T
  type element

    sharing type Native.T = Big.T = Little.T
        and type T = U_Big.T = U_Little.T
        and type F_Big.T = F_Little.T
        and type R_Big.T = R_Little.T
        and type element = Native.element = Big.element = Little.element
	       = U_Big.element = U_Little.element
	       = F_Big.element = F_Little.element
	       = R_Big.element = R_Little.element

  val native_big_endian: bool

(* conversion to aligned from unaligned may truncate the array at both ends *)
  val align: T -> Native.T
  val unalign: Native.T -> T

(* conversion to forward-aligned from unaligned may cut off some
   data at the start of the array. *)
  val align_f: T -> F_Big.T
  val unalign_f: F_Big.T -> T

(* conversion to reverse-aligned from unaligned may cut off some
   data at the end of the array. *)
  val align_r: T -> R_Big.T
  val unalign_r: R_Big.T -> T

 end (* sig *)

(*
		11.	signature WORD_ARRAY

	Arrays at all sizes that are power-of-two bytes, and corresponding
	conversion functions.
*)

signature WORD_ARRAY =
 sig
  type T
(*
  structure W1  : BYTE_ACCESS_ARRAY
  structure W2  : BYTE_ACCESS_ARRAY
  structure W4  : BYTE_ACCESS_ARRAY
*)
  structure W8  : BYTE_ACCESS_ARRAY where type element = Word8.word
  structure W16 : BYTE_ACCESS_ARRAY where type element = Word16.word
  structure W32 : BYTE_ACCESS_ARRAY where type element = Word32.word
  structure W64 : BYTE_ACCESS_ARRAY where type element = Word64.word
  structure W128: BYTE_ACCESS_ARRAY where type element = Word128.word
  structure W256: BYTE_ACCESS_ARRAY where type element = Word256.word

(*
  val from1  : W1.T   -> T
  val from2  : W2.T   -> T
  val from4  : W4.T   -> T
*)
  val from8  : W8.T   -> T
  val from16 : W16.T  -> T
  val from32 : W32.T  -> T
  val from64 : W64.T  -> T
  val from128: W128.T -> T
  val from256: W256.T -> T

(*
  val to1  : T -> W1.T
  val to2  : T -> W2.T
  val to4  : T -> W4.T
*)
  val to8  : T -> W8.T
  val to16 : T -> W16.T
  val to32 : T -> W32.T
  val to64 : T -> W64.T
  val to128: T -> W128.T
  val to256: T -> W256.T

  val alignment_f: T -> Word.word
  val alignment_r: T -> Word.word

 end (* sig *)

(*
		12.	signature EXPOSE_WORD_ARRAY

		This is like a WORD_ARRAY, but provides a handle to
		the actual physical underlying array.  This is
		used mostly for raw I/O.
*)

signature EXPOSE_WORD_ARRAY =
 sig
  include WORD_ARRAY

  val expose: T -> Word8Array.array * Word.word (* first *)
                 * Word.word (* last *)
 end


