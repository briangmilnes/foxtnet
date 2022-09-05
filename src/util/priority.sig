(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edoardo Biagioni (Edoardo.Biagioni@cs.cmu.edu)
	Brian Milnes (Brian.Milnes@cs.cmu.edu)
	Nick Haines  (Nick.Haines@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

		i.	Abstract

	A priority queue.

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	signature PRIORITY_QUEUE

	iii.	RCS Log
	
$Log: priority.sig,v $
Revision 1.8  1996/06/11  03:35:22  esb
new, side-effecting interface.

Revision 1.7  1994/01/17  17:54:12  esb
Standardized the interface.

Revision 1.6  1994/01/13  16:18:44  milnes
Updated on the path to updating coroutines with a delete.

	1.	signature PRIORITY_QUEUE
*)

signature PRIORITY_QUEUE =
 sig
  type key
  type T        (* The type of the priority queue. *)

  val new: unit -> T 
  val size: T -> int		(* The number of objects in the queue. *)
  val empty: T -> bool		(* Is this priority queue empty ? *)

  (* Add an element to the priority queue via a side effect. *)
  (* Return a function to delete that element.               *)
  (* If the delete function is called more than once, or after the element   *)
  (* has been removed via next or pop, the Not_In_Queue exception is raised. *)
  val add: T * key -> (unit -> unit)
  exception Not_In_Queue

  (* Return the minimum item in the priority queue. *)
  val first: T -> key option  

  (* Return the minimum item in the priority queue.  Delete this item *)
  (* from the priority queue via a side effect.                       *)
  val next: T -> key option

  (* Delete the minimum item in the priority queue with a side effect. *)
  val pop: T -> unit

  (* Physically remove all deleted elements. *)
  val cleanup : T -> unit

  val fold: (key * 'b -> 'b) -> T -> 'b -> 'b

  val makestring: T * (key -> string) * string (* separator *) -> string

 end (* sig *)

