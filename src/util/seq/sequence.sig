(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edoardo Biagioni (Edoardo.Biagioni@cs.cmu.edu)
	Ken Cline (Kenneth.Cline@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15213-3891

		i.	Abstract

	sequence.sig: signature for sequence type.

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	signature SEQUENCE
	2.	signature CONVERT_SEQUENCE
	3.	signature FINITE_SEQUENCE
	4.	signature COMPARE_SEQUENCE
	5.	signature LIST_SEQUENCE
	6.	signature STREAM_SEQUENCE
	7.	signature CHANNEL_SEQUENCE
	8.	signature ARRAY_SEQUENCE
	9.	signature REV_ARRAY_SEQUENCE

		iii.	RCS Log
	
$Log: sequence.sig,v $
Revision 1.1  1995/11/12  16:43:29  esb
Initial revision


		1.	signature SEQUENCE

	A sequence is a data structure containing a sequence of
	elements.  There is a variety of possible sequences; some
	sequences may be infinite in length (lazy) or updatable.
	For each defined sequence, there may be a variety of
	possible implementations.
*)

signature SEQUENCE =
 sig
  type 'a T

  val new: ('b -> ('a * 'b) option) -> 'b -> 'a T

  val next: 'a T -> ('a * 'a T) option
  val seek: 'a T * int -> 'a T

  exception Empty
  val head: 'a T -> 'a			(* may raise Empty *)
  val tail: 'a T -> 'a T		(* may raise Empty *)
  val nth: 'a T * int -> 'a		(* may raise Empty *)

  val isempty: 'a T -> bool		(* O(1) *)
  val wait: 'a T -> unit		(* O(n), may not terminate *)

  val map: ('a -> 'b) -> 'a T -> 'b T
  val app: ('a -> unit) -> 'a T -> unit
  val fold: ('a * 'b -> 'b) -> 'b -> 'a T -> 'b
  val filter: ('a -> 'b option) -> 'a T -> 'b T
 end

(*
		2.	signature CONVERT_SEQUENCE

	A generic conversion module from any one sequence to any
	other sequence.  If both structures are lazy/nonterminating,
	the implementation will preserve laziness/nontermination.
*)

signature CONVERT_SEQUENCE =
 sig
  structure From: SEQUENCE
  structure To: SEQUENCE

  val convert: 'a From.T -> 'a To.T
 end

(*
		3.	signature FINITE_SEQUENCE
*)

signature FINITE_SEQUENCE =
 sig
  include SEQUENCE

  val create: 'a * int -> 'a T
  val tabulate: (int -> 'a) * int -> 'a T

  val equal: 'a T * 'a T * ('a * 'a -> bool) -> bool
  val less: 'a T * 'a T * ('a * 'a -> bool) -> bool

  val length: 'a T -> int
  val append: 'a T * 'a T -> 'a T
  val concat: 'a T T -> 'a T
  val reverse: 'a T -> 'a T
 end

(*
		4.	signature COMPARE_SEQUENCE

	Gives a description of the differences between two finite sequences.
*)

signature COMPARE_SEQUENCE =
 sig
  type 'a T
  val compare: 'a T * 'a T * ('a -> string) -> string
 end

(*
		5.	signature LIST_SEQUENCE
*)

signature LIST_SEQUENCE =
 sig
  include FINITE_SEQUENCE

  datatype 'a list = nil | :: of 'a * 'a list
  sharing type T = list
 end

(*
		6.	signature STREAM_SEQUENCE

	"Purely functional" streams (most implementations will be
	memoized).
*)

signature STREAM_SEQUENCE =
 sig
  include SEQUENCE

  val empty: 'a T
  val cons: 'a * 'a T -> 'a T
  val lazy_cons: 'a * (unit -> 'a T) -> 'a T
  val delay: (unit -> 'a T) -> 'a T
 end

(*
		7.	signature CHANNEL_SEQUENCE

	Imperative channels.
*)

signature CHANNEL_SEQUENCE =
 sig
  include SEQUENCE

  val create: unit -> 'a T
  val enqueue: 'a * 'a T -> unit
  val close: 'a T -> unit
  val size: 'a T -> int
  val flush: 'a T -> 'a T
 end

(*
		8.	signature ARRAY_SEQUENCE

	Updatable fixed-size sequences.
*)

signature ARRAY_SEQUENCE =
 sig
  include FINITE_SEQUENCE

  type 'a U

  val write: 'a T -> 'a U option
  val read: 'a U -> 'a T
  val update: 'a U * 'a -> 'a U option
 end (* sig *)

(*
		9.	signature REV_ARRAY_SEQUENCE

	Updatable fixed-size sequences accessible at both ends.
*)

signature REV_ARRAY_SEQUENCE =
 sig
  type T
  type element
  type U

  structure F: ARRAY_SEQUENCE
  structure R: ARRAY_SEQUENCE
(*  not legal ML
   sharing type 'a T = 'a R.T = 'a F.T
       and type 'a U = 'a R.U = 'a F.U
*)
 end (* sig *)

