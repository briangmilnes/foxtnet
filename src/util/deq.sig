(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edoardo Biagioni (Edoardo.Biagioni@cs.cmu.edu)
	Brian Milnes (Brian.Milnes@cs.cmu.edu)
	Ken Cline (Kenneth.Cline@cs.cmu.edu)
        Nick Haines (Nick.Haines@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

		i.	Abstract

	A DEQ is a double ended queue with addition to front or back, and
  removal from the front, with a general delete operation.


		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	signature DEQ

		I.	RCS Log
	
$Log: deq.sig,v $
Revision 1.2  1994/01/17  17:55:30  esb
Standardized the interface.

Revision 1.1  1994/01/15  00:18:04  esb
Initial revision

		1.	signature DEQ
*)

signature DEQ =
 sig
  type 'a T 
 
  (* Get a new empty deque. *)
  val new: unit ->  'a T

  (* Add elements to either the front or the back of the DEQ. *)
  val add_to_front: 'a T * 'a -> 'a T
  val add_to_back:  'a T * 'a -> 'a T

  (* Check if the DEQ is empty. *)
  val empty: 'a T -> bool
  val size: 'a T -> int

  (* Take one element from the front of the DEQ. *)
  val first: 'a T -> ('a T * 'a) option
  (* Take one element from the back of the DEQ. *)
  val last: 'a T -> ('a T * 'a) option

  val delete: 'a T * ('a -> bool) -> 'a T option

  val map: ('a -> 'b) -> 'a T -> 'b T

  val fold: ('a * 'b -> 'b) -> 'a T -> 'b -> 'b

  val makestring: 'a T * ('a -> string) * string (* separator *) -> string
 end 

