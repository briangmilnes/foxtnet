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

	A setup for constructing and comparing dynamic arrays for testing
	purposes.

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor Compare_Dyn_Array
	2.	types difference and data
	3.	function create
	4.	function compare
	5.	function makestring


		iii.	RCS Log
	
$Log: comparedyn.fun,v $
Revision 1.7  1995/06/20  17:29:28  esb
modernized COMPARE_ARRAY signature.

Revision 1.6  1995/03/10  03:51:29  esb
adapted to new vendor.sig.

Revision 1.5  1995/02/13  23:00:43  esb
simplified.

Revision 1.4  1995/02/04  21:47:05  robby
updated to 107

Revision 1.3  1994/09/30  16:25:52  esb
changed to work with SML/NJ 1.05 as well as 0.93.

Revision 1.2  1994/08/19  18:02:04  esb
added RCS log.

		1.	functor Compare_Dyn_Array
*)

functor Compare_Dyn_Array (structure Dynamic_Array: DYNAMIC_BYTE_ARRAY
			   structure Debug: DEBUG
			   structure V: VENDOR
			   structure Create: CREATE): COMPARE_ARRAY  =
 struct 
  structure Trace = Trace (structure V = V
			   val debug_level = NONE
			   val module_name = "comparedyn.fun"
			   val makestring = fn _ => NONE)
  val local_print = Trace.local_print

(*
		2.	types difference and data
*)

  datatype difference = Data_Difference of int * FoxWord8.word * FoxWord8.word 
                      | Data_Size of int * int

  type data = Dynamic_Array.T

(*
		3.	function create

	Create a Dynamic_Array.T with the given header and
	data bytes filled out.
*)

  val create = Dynamic_Array.init_list1

(*
		4.	function compare
*)
 
  val get_byte = Dynamic_Array.sub1

  fun compare this that =
       let val this_len = Dynamic_Array.size this
           val that_len = Dynamic_Array.size that
	   fun loop (~1, this, that) = []
             | loop (n, this, that) = 
                if get_byte (this, n) <> get_byte (that, n) then
	         Data_Difference (n, get_byte (this, n), get_byte (that, n)) ::
	         loop (n - 1, this, that)
                else loop (n - 1, this, that)
           val minimum_amount_of_data = Integer.min (this_len, that_len)
      in if (this_len <> that_len) then 
          Data_Size (this_len, that_len) ::
          loop (minimum_amount_of_data - 1, this, that)
         else loop (minimum_amount_of_data - 1, this, that)
      end

(*
		5.	function makestring
*)

  fun makestring [] = ""
    | makestring (head :: rest) = 
       (case head of
           Data_Difference (i, b1, b2) =>
	    "Data_Difference " ^ V.Integer.makestring i ^ " " ^
	    FoxMakestring.word8 b1 ^ " " ^ FoxMakestring.word8 b2 ^
	    makestring rest
	 | Data_Size (i1, i2) =>
	    "Data_Size " ^
	    V.Integer.makestring i1 ^ " " ^ V.Integer.makestring i2 ^
	    makestring rest)

 end (* struct *)



