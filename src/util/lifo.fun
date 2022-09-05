(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edo Biagioni (esb@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

	i.	Abstract

	lifo.fun: functor Fifo which produces a FIFO structure.

$Log: lifo.fun,v $
Revision 1.3  1994/03/02  18:37:59  esb
added RCS log.


*)

functor Lifo () : LIFO =
 struct

  type 'a T = 'a list

  fun new () = []

  val size = length

  fun push (s, elt) = elt :: s

  fun pop [] = NONE
    | pop (head :: rest) = SOME (rest, head)

  val map = List.map

  val fold = List.fold

  fun makestring (store, elstring, separator) =
       let fun foldstring (element, "") = elstring element
	     | foldstring (element, prev) =
	        (elstring element) ^ separator ^ prev
       in fold foldstring store ""
       end

 end (* struct *)

