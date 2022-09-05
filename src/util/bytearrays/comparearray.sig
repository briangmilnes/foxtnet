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

	A setup for constructing and comparing packets for testing purposes.



		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	signature COMPARE_MESSAGE


		iii.	RCS Log
	
$Log: comparearray.sig,v $
Revision 1.3  1995/06/20  17:29:09  esb
modernized COMPARE_ARRAY signature.

Revision 1.2  1994/09/30  16:25:52  esb
changed to work with SML/NJ 1.05 as well as 0.93.

Revision 1.1  1994/08/02  20:17:30  esb
Initial revision


		1.	signature COMPARE_ARRAY
*)

signature COMPARE_ARRAY =
 sig
  datatype difference = Data_Difference of int * FoxWord8.word * FoxWord8.word 
                      | Data_Size of int * int

  type data

  val create: FoxWord8.word list -> data
             (* data_bytes -> T *)

  val compare: data -> data -> difference list 

  val makestring: difference list -> string
 end


