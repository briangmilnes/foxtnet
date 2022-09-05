(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edoardo Biagioni <Edoardo.Biagioni@cs.cmu.edu>
	Brian Milnes (Brian.Milnes@cs.cmu.edu)
	Nick Haines  (Nick.Haines@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

	i.	Abstract
	fifo.sig: signature FIFO. A fifo is used to store objects
	and retrieve them in the same order in which they were
	stored (First-In-First-Out).

	ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	signature FIFO

	iii.	RCS Log
	
$Log: fifo.sig,v $
Revision 1.7  1994/05/04  01:37:18  esb
added the "empty" function.

Revision 1.6  94/04/26  17:51:20  esb
added a delete function.

Revision 1.5  94/01/17  17:54:12  esb
Standardized the interface.

Revision 1.4  1993/10/28  17:17:17  esb
added map and fold.

Revision 1.3  1993/10/12  22:36:09  esb
changed order of arguments so the fifo is always the first argument.

Revision 1.2  1993/10/05  21:22:31  esb
improved the abstract.

Revision 1.1  1993/06/10  22:41:20  milnes
Initial revision


		1.	signature FIFO
*)

signature FIFO =
 sig
  type 'a T		(* the type of the fifo itself *)

  val new: unit -> 'a T	(* create a new, empty fifo *)
  val size: 'a T -> int
  val empty: 'a T -> bool

  (* add an element at the end of the fifo *)
  val add: 'a T * 'a -> 'a T

  val next: 'a T -> ('a T * 'a) option

  (* delete only returns a new FIFO if something was deleted. *)
  val delete: 'a T * ('a -> bool) -> 'a T option

  val map: ('a -> 'b) -> 'a T -> 'b T

  val fold: ('a * 'b -> 'b) -> 'a T -> 'b -> 'b

  val makestring: 'a T * ('a -> string) * string (* separator *) -> string

 end
