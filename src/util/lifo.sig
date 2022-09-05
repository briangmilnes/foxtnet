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
	lifo.sig: signature LIFO. A lifo is used to store objects
	and retreive them in Last-In-First-Out fashion.

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	signature LIFO

		iii.	RCS Log
	
$Log: lifo.sig,v $
Revision 1.3  1994/01/17  17:54:12  esb
Standardized the interface.

Revision 1.2  1993/10/05  21:22:31  esb
improved the abstract.

Revision 1.1  1993/06/10  22:41:20  milnes
Initial revision


		1.	signature LIFO
*)

signature LIFO =
 sig
  type 'a T		(* the type of the lifo itself *)

  val new   : unit -> 'a T	(* create a new, empty lifo *)
  val size  : 'a T -> int

  (* place an element at the head of the lifo *)
  val push  : 'a T * 'a -> 'a T

  val pop   : 'a T -> ('a T * 'a) option

  val map: ('a -> 'b) -> 'a T -> 'b T

  val fold: ('a * 'b -> 'b) -> 'a T -> 'b -> 'b

  val makestring: 'a T * ('a -> string) * string (* separator *) -> string

 end
