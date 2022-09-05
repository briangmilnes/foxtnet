(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edoardo Biagioni (Edoardo.Biagioni@cs.cmu.edu)
	Brian Milnes (Brian.Milnes@cs.cmu.edu)
	Ken Cline (Kenneth.Cline@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

		i.	Abstract

	Efficient byte-ordering functions in ML.

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	signature BYTE_ORDER
	2.	signature BYTE_ORDERS

		iii.	RCS Log
	
$Log: order.sig,v $
Revision 1.4  1997/11/14  11:54:52  cline
changed rigid sharing constraing to where type

Revision 1.3  96/01/15  18:00:49  cline
removed FoxWord

Revision 1.2  1994/09/30  16:36:02  esb
changed ubytes to words

Revision 1.1  1994/02/21  00:19:45  esb
Initial revision


	1.	signature BYTE_ORDER
*)

signature BYTE_ORDER =
 sig

  type T    (* the type on which the reordering operations are defined *)

  val to_little: T -> T		(* machine byte order to little-endian *)
  val to_big: T -> T		(* machine byte order to big-endian *)
  val from_little: T -> T	(* little-endian to machine byte order *)
  val from_big: T -> T		(* big-endian to machine byte order *)
  val invert: T -> T		(* big-endian to little-endian or viceversa *)

 end (* sig *)

(*
	2.	signature BYTE_ORDERS
*)

signature BYTE_ORDERS =
 sig
  structure B1: BYTE_ORDER where type T = Word8.word
  structure B2: BYTE_ORDER where type T = Word16.word
  structure B4: BYTE_ORDER where type T = Word32.word
 end (* sig *)
