(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edoardo Biagioni (Edoardo.Biagioni@cs.cmu.edu)
	Ken Cline (Kenneth.Cline@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15213-3891

		i.	Abstract

	seq.sig: signature for sequence type.

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor Convert_Sequence 
	2.	functor From_List
	3.	structure List_Sequence

		iii.	RCS Log
	
$Log: sequence.fun,v $
Revision 1.4  1997/11/14  11:56:24  cline
removed locally declared signature

Revision 1.3  97/03/06  15:53:55  cline
removed structure Stream_Sequence.

Revision 1.2  1996/10/13  18:50:34  esb
adapted to value restriction.

Revision 1.1  1995/11/12  16:45:54  esb
Initial revision

Revision 1.3  1995/07/05  17:47:43  esb
changed the definition of new to eliminate generator and iterator.

Revision 1.2  1995/06/29  18:23:18  esb
adapted to new seq.sig

Revision 1.1  1995/06/20  17:32:56  esb
Initial revision


		1.	functor Convert_Sequence 
*)

functor Convert_Sequence (structure From: SEQUENCE
			  structure To: SEQUENCE): CONVERT_SEQUENCE =
 struct
  structure From = From
  structure To = To

  fun convert from = To.new From.next from

 end (* struct *)

(*
		2.	functor From_List
*)

functor From_List (structure List: LIST): LIST_SEQUENCE =
 struct
  structure Basics =
   struct
    local
     structure S: sig datatype 'a list = nil | :: of 'a * 'a list
		       exception Empty
		  end = List
    in
     open S
     infix ::
    end
    type 'a T = 'a list

   end

  open Basics

  fun new f seq =
       case f seq of
          NONE => List.nil
	| SOME (element, new_seq) => List.:: (element, new f new_seq)

  fun create (_, 0) = List.nil
    | create (value, count) = List.:: (value, create (value, count - 1))

  fun tabulate (element_fun, size) = List.tabulate (size, element_fun)

  fun next nil = NONE
    | next (op :: result) = SOME result

  fun seek (list, count) = ((List.drop (list, count)) handle _ => nil)

  val head = List.hd
  val tail = List.tl
  fun nth arg = ((List.nth arg) handle _ => raise Empty)

  fun isempty List.nil = true
    | isempty _ = false

  val length = List.length

  fun wait _ = ()

  val map = List.map
  val app = List.app
  val fold = List.foldr
  val append = List.@
  val concat = List.concat
  val reverse = List.rev
  val filter = List.mapPartial

  fun equal (nil, nil, _) = true
    | equal (nil, _, _) = false
    | equal (_, nil, _) = false
    | equal (first1 :: rest1, first2 :: rest2, eq) =
       eq (first1, first2) andalso equal (rest1, rest2, eq)

  fun less (nil, nil, _) = false
    | less (nil, _, _) = true
    | less (_, nil, _) = false
    | less (first1 :: rest1, first2 :: rest2, less_elt) =
       less_elt (first1, first2) orelse
       (not (less_elt (first2, first1))	(* first1 = first2 *)
	andalso less (rest1, rest2, less_elt))
 end (* struct *)

(*
		3.	structure List_Sequence
*)

structure List_Sequence = From_List (structure List = List)
