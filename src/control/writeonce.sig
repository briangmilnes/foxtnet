(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edoardo Biagioni (esb@cs.cmu.edu)
	Ken Cline (Kenneth.Cline@cs.cmu.edu)
	Sidd Puri (sidd@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

		i.	Abstract

	writeonce.sig: signature for write-once variables.

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	signature WRITE_ONCE
	2.	signature MEMOIZE

		iii.	RCS Log

$Log: writeonce.sig,v $
Revision 1.4  1997/02/13  00:48:12  esb
eliminated imperative types.

Revision 1.3  1996/02/06  22:07:34  esb
added Memoize.

Revision 1.2  1995/08/16  21:31:42  esb
added comment

Revision 1.1  1995/08/16  21:26:05  esb
Initial revision


		1.	signature WRITE_ONCE

	A write-once variable can be written to exactly once.
	Any access to the variable before it is written is
	suspended, and resumed once the variable is written.
*)

signature WRITE_ONCE =
 sig
  type 'a T

  val new: unit -> 'a T
  val get: 'a T -> 'a

  exception Already_Initialized
  val set: 'a T * 'a -> unit
 end

(*
		2.	signature MEMOIZE

	A memo-ized variable is initialized at the time it
	is first needed, and never recomputed.
*)

signature MEMOIZE =
 sig
  type ('a, 'b) T

  val new: (('a -> 'b) * 'a) -> ('a, 'b) T 
  val get: ('a, 'b) T -> 'b

 end
