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
	1.	functor Compare_Byte_Arrays
	2.	types difference and data
	3.	function create
	4.	function compare
	5.	function makestring


		iii.	RCS Log
	
$Log: comparebyte.fun,v $
Revision 1.4  1995/06/20  17:29:28  esb
modernized COMPARE_ARRAY signature.

Revision 1.3  1995/03/10  03:51:29  esb
adapted to new vendor.sig.

Revision 1.2  1994/09/30  16:25:52  esb
changed to work with SML/NJ 1.05 as well as 0.93.

Revision 1.1  1994/08/02  20:17:30  esb
Initial revision


		1.	functor Compare_Byte_Arrays
*)

functor Compare_Byte_Arrays (structure Debug: DEBUG
			     structure V: VENDOR
			     structure Create: CREATE): COMPARE_ARRAY  =
 struct 
  structure Trace = Trace (structure V = V
			   val debug_level = NONE
			   val module_name = "comparebyte.fun"
			   val makestring = fn _ => NONE)
  val local_print = Trace.local_print

(*
		2.	types difference and data
*)

  datatype difference = Data_Difference of int * FoxWord8.word * FoxWord8.word 
                      | Data_Size of int * int   

  type data = ByteArray.bytearray

(*
		3.	function create

	Create an array with the given header and data bytes filled out.
*)

  fun create data_bytes =
       let val data_size = List.length data_bytes
           val b = Create.create data_size
           fun dolist (offset, [], b) = b
             | dolist (offset, a :: l, b) = (FoxWord8.update (b, offset, a);
					     dolist (offset + 1, l, b))
       in dolist (0, data_bytes, b)
       end
 
(*
		4.	function compare
*)

  fun compare this that =
       let val this_length = ByteArray.length this
           val that_length = ByteArray.length that
           fun loop (0, this, that) = 
                if (FoxWord8.sub (this,0)) <>
	           (FoxWord8.sub (that,0)) then
	          [Data_Difference (0, (FoxWord8.sub (this, 0)),
				    (FoxWord8.sub (that, 0)))]
                else []
             | loop (n, this, that) = 
                if FoxWord8.sub (this, n) <> FoxWord8.sub (that, n) then
	         Data_Difference (n, (FoxWord8.sub (this,n)),
				  (FoxWord8.sub (that,n))) ::
	         loop (n - 1, this, that)
                else loop (n - 1, this, that)
      in if this_length <> that_length then 
          Data_Size (this_length, that_length) ::
          loop (Integer.min (this_length, that_length) - 1, this, that)
         else loop (this_length - 1, this, that)
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
