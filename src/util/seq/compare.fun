(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Brian Milnes (Brian.Milnes@cs.cmu.edu)
	Nick Haines  (Nick.Haines@cs.cmu.edu)
	Edoardo Biagioni <Edoardo.Biagioni@cs.cmu.edu>
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

		i.	Abstract

	Two functors for comparing sequences.

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor Compare_Seq


		iii.	RCS Log
	
$Log: compare.fun,v $
Revision 1.3  1996/01/19  23:07:31  esb
adapted to the new wordarray signature.

Revision 1.2  1996/01/15  18:00:49  cline
removed FoxWord

Revision 1.1  1995/06/20  17:32:56  esb
Initial revision


		1.	functor Compare_Seq
*)

functor Compare_Seq (structure Seq: FINITE_SEQ
		     val equal: Seq.element * Seq.element -> bool
		     val makestring: Seq.element -> string
		     structure V: VENDOR): COMPARE_SEQ =
 struct
  type T = Seq.T

  datatype difference = Data_Difference of int * Seq.element * Seq.element 
                      | Data_Size of int * int

  fun size_difference (left, right) =
       if left = right then ""
       else
        "sizes (" ^ Word.toString left ^ ", " ^
	Word.toString right ^ "), "

  fun data_difference (left, right, pos) =
       if equal (left, right) then ""
       else
	"data@" ^ Integer.toString pos ^ " ( " ^
	makestring left ^ ", " ^ makestring right ^ "), "

  fun compare_loop (NONE, NONE, _) = ""
    | compare_loop (SOME (first1, rest1), SOME (first2, rest2), pos) =
       data_difference (first1, first2, pos) ^
       compare_loop (Seq.next rest1, Seq.next rest2, pos + 1)
    | compare_loop _ = ""

  fun compare (data1, data2) =
       (size_difference (Seq.length data1, Seq.length data2) ^
	compare_loop (Seq.next data1, Seq.next data2, 0))

 end (* struct *)
