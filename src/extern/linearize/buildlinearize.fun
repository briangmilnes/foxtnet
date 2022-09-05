 (*

	FoxNet:  The Fox Project's Communication Protocol Implementation Effort
	Edoardo Biagioni  (Edoardo.Biagioni@cs.cmu.edu)
	Brian Milnes  (Brian.Milnes@cs.cmu.edu)
	Ken Cline  (Kenneth.Cline@cs.cmu.edu)
        Nick Haines  (Nick.Haines@cs.cmu.edu)
	Robert Findler  (Robert.Findler@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

		i.	Abstract



		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor Build_Linearize

		iii.	RCS Log
	
$Log: buildlinearize.fun,v $
Revision 1.2  1995/03/10  03:53:53  esb
adapted to new vendor.sig.

Revision 1.1  1994/08/25  23: 43: 03  robby
Initial revision


		1.	functor Build_Linearize
*)

functor Build_Linearize (structure Copy: COPY
			 structure Access: ACCESS
			 structure Orders: BYTE_ORDERS
			 structure Create: CREATE
			 structure V: VENDOR
			 structure Format: FORMAT
			 val debug_level: int ref option): BUILD_LINEARIZE = 
 struct

  structure Ubyte1 = Ubyte1 (val swap = false
			     val debug_level = debug_level
			     structure V = V)
  structure Ubyte2 = Ubyte2 (val swap = false
			     val debug_level = debug_level
			     structure Order = Orders.B2
			     structure V = V)
  structure Ubyte4 = Ubyte4 (val swap = false
			     val debug_level = debug_level
			     structure Order = Orders.B4
			     structure V = V)
  structure Int = Int (structure Ubyte4 = Ubyte4)

  structure Rev_Ubyte2 = Ubyte2 (val swap = true
				 val debug_level = debug_level
				 structure Order = Orders.B2
				 structure V = V)
  structure Rev_Ubyte4 = Ubyte4 (val swap = true
				 val debug_level = debug_level
				 structure Order = Orders.B4
				 structure V = V)
  structure Rev_Int = Int (structure Ubyte4 = Rev_Ubyte4)

  structure CString = CString (structure Access = Access
			       structure V = V
			       val debug_level = debug_level)

  structure ByteArray1 = ByteArray1_Linearize (structure Copy = Copy
					       structure Create = Create)
  structure ByteArray2 = ByteArray2_Linearize (structure V = V
					       structure Copy = Copy
					       structure Create = Create
					       structure Ubyte2 = Ubyte2
					       structure Format = Format
					       val debug_level = debug_level)
  structure ByteArray3 = ByteArray3_Linearize  (structure Ubyte1 = Ubyte1
						structure V = V
						structure Copy = Copy
						structure Create = Create
						structure Format = Format
						val debug_level = debug_level)
  structure ByteArray4 = ByteArray4_Linearize (structure Ubyte1 = Ubyte1
					       structure V = V
					       structure Copy = Copy
					       structure Create = Create
					       structure Format = Format
					       val debug_level = debug_level)
  structure Rev_ByteArray2 =
      ByteArray2_Linearize (structure V = V
			    structure Copy = Copy
			    structure Create = Create
			    structure Ubyte2 = Rev_Ubyte2
			    structure Format = Format
			    val debug_level = debug_level)
end
