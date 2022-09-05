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
	removal from the front or the back, with a general delete operation.

	ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor DEQ
	2.	function new
	3.	function add_to_front
	4.	function add_to_back
	5.	function empty
	6.	function size
	7.	function first
	8.	function last
	9.	function delete
	10.	function map
	11.	function fold
	12.	function makestring


	iii.	RCS Log
	
$Log: deq.fun,v $
Revision 1.4  1996/07/23  15:03:18  cline
*** empty log message ***

Revision 1.3  1995/06/20  17:37:12  esb
minor fix.

Revision 1.2  1994/01/17  17:55:30  esb
Standardized the interface.

Revision 1.1  1994/01/15  00:18:04  esb
Initial revision

	1.	functor DEQ
*)

functor Deq (structure V: VENDOR) : DEQ =
 struct
  datatype 'a T = Deq of 'a list * 'a list
  
  (* A two list representation of double-ended queues: the first
     list is the front elements of the queue in normal order,
     the second list is the back elements of the queue in
     reversed order. *)

(*
	2.	function new
*)

  fun new () = Deq ([], [])

(*
	3.	function add_to_front
*)

  fun add_to_front (Deq (front, back), object) = Deq (object :: front, back)

(*
	4.	function add_to_back
*)

  fun add_to_back (Deq (front, back), object) = Deq (front, object :: back)
  
(*
	5.	function empty
*)

  fun empty (Deq ([], [])) = true
    | empty _ = false

(*
	6.	function size
*)

  fun size (Deq (front, back)) = length front + length back

(*
	7.	function first
*)

  fun first (Deq (head :: rest, back)) = SOME (Deq (rest, back), head)
    | first (Deq ([], [])) = NONE 
    | first (Deq ([], back))= first (Deq (rev back, []))
   
(*
	8.	function last
*)

  fun last (Deq (front, head :: rest)) = SOME (Deq (front, rest), head)
    | last (Deq ([], [])) = NONE 
    | last (Deq (front, []))= first (Deq ([], rev front))
   
(*
	9.	function delete
*)

  fun delete (Deq (front, back), match) = 
       let fun remove [] = NONE
	     | remove (head :: rest) =
	        if match head then SOME rest else remove rest
       in case remove front of
	     NONE =>
	      (case remove back of
		  NONE => NONE
		| SOME new_back => SOME (Deq (front, new_back)))
	   | SOME new_front => SOME (Deq (new_front, back))
       end

(*
	10.	function map
*)

  fun map f (Deq (front, back)) =
       Deq (List.map f (front @ rev back), [])

(*
	11.	function fold
*)

  fun fold f (Deq (front, back)) =
       V.List.fold f (front @ rev back)

(*
	12.	function makestring
*)

  fun makestring (deq, f, separator) =
       let fun foldseparate (object, "") = f object
	     | foldseparate (object, string) = f object ^ separator ^ string
       in fold foldseparate deq ""
       end

 end



 
