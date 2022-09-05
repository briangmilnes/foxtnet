(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edo Biagioni (esb@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

	i.	Abstract

	fifo.fun: functor Fifo which produces a FIFO structure.

	ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor Fifo
	2.	function new
	3.	function size
	4.	function empty
	5.	function add
	6.	function next
	7.	function delete
	8.	function map
	9.	function fold
	10.	function makestring

	iii.	RCS Log

$Log: fifo.fun,v $
Revision 1.9  1996/07/22  21:22:22  cline
*** empty log message ***

Revision 1.8  1995/06/20  17:37:26  esb
minor fix.

Revision 1.7  1994/05/10  07:42:31  esb
fixed empty, which always returned false.

Revision 1.6  94/05/04  01:37:18  esb
added the "empty" function.

Revision 1.5  94/04/26  17:51:20  esb
added a delete function.

Revision 1.4  94/01/17  17:54:12  esb
Standardized the interface.

	1.	functor Fifo
*)

functor Fifo (structure V: VENDOR) : FIFO =
 struct

  datatype 'a T = Fifo of 'a list * 'a list

(*
	2.	function new
*)

  fun new () = Fifo ([], [])

(*
	3.	function size
*)

  fun size (Fifo (fwd, back)) = length fwd + length back

(*
	4.	function empty
*)

  fun empty (Fifo ([], [])) = true
    | empty _ = false

(*
	5.	function add
*)

  fun add (Fifo (fwd, back), elt) = Fifo (fwd, elt :: back)

(*
	6.	function next
*)

  fun next (Fifo ([], [])) = NONE
    | next (Fifo ([], back)) = next (Fifo (rev back, []))
    | next (Fifo (h :: r, back)) = SOME (Fifo (r, back), h)

(*
	7.	function delete
*)

  local
   fun any_true (a, b) = a orelse b
   fun keep_if_false f (a, b) = if f a then b else a :: b
  in
   fun delete (Fifo (front, back), f) =
        let val elements = front @ rev back
	    val deletes = V.List.map f elements
	    val any = V.List.fold any_true deletes false
        in if any then
	    SOME (Fifo (V.List.fold (keep_if_false f) elements [], []))
	   else NONE
        end
  end (* local *)

(*
	8.	function map

	The result Fifo has all its elements in the "front" component.
*)

  fun map f (Fifo (front, back)) = Fifo (List.map f (front @ rev back), [])

(*
	9.	function fold

	The result Fifo has all its elements in the "front" component.
*)

  fun fold f (Fifo (front, back)) init =
       let fun foldlist (element, rest) = f (element, rest)
       in V.List.fold foldlist (front @ rev back) init
       end

(*
	10.	function makestring
*)

  fun makestring (store, elstring, separator) =
       let fun foldstring (element, "") = elstring element
	     | foldstring (element, prev) =
	        (elstring element) ^ separator ^ prev
       in fold foldstring store ""
       end

 end (* struct *)

