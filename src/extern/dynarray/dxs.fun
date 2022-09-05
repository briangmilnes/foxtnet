(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edoardo Biagioni (Edoardo.Biagioni@cs.cmu.edu)
	Brian Milnes (Brian.Milnes@cs.cmu.edu)
	Ken Cline (Kenneth.Cline@cs.cmu.edu)
        Nick Haines (Nick.Haines@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15213-3891

		i.	Abstract
	
	A functor for the DXS.



		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	signature DXS

		iii.	RCS Log
	
$Log: dxs.fun,v $
Revision 1.4  1995/01/14  02:26:39  esb
tested and fixed the byte ordering (was backwards).

Revision 1.3  1994/10/11  15:43:04  cline
Upgraded to work with SML/NJ 1.05b

Revision 1.2  1994/09/30  16:56:04  esb
changed DXS to BYTE_EXTERN.

Revision 1.1  1994/08/12  14:26:45  milnes
Initial revision


		1.	signature DXS
*)

functor Byte_Extern (structure Dyn_Array: DYNAMIC_BYTE_ARRAY
		     structure Order: BYTE_ORDERS): BYTE_EXTERN = 
 struct

  val update2 = case FoxWord16.endian () of
                   FoxWord16.Big => Dyn_Array.update2
		 | FoxWord16.Little =>
		    fn (a, i, n) => Dyn_Array.update2 (a, i, Order.B2.invert n)

  val sub2 = case FoxWord16.endian () of
                FoxWord16.Big => Dyn_Array.sub2
              | FoxWord16.Little =>
		 fn arg => Order.B2.invert (Dyn_Array.sub2 arg)

  val update4 = case FoxWord32.endian () of
                   FoxWord32.Big => Dyn_Array.update4
		 | FoxWord32.Little =>
		    fn (a, i, n) => Dyn_Array.update4 (a, i, Order.B4.invert n)

  val sub4 = case FoxWord32.endian () of
                FoxWord32.Big => Dyn_Array.sub4
              | FoxWord32.Little =>
		 fn arg => Order.B4.invert (Dyn_Array.sub4 arg)

  structure Byte1 = DXByteN (structure Dyn_Array = Dyn_Array
			     type byteN = FoxWord8.word
			     val n = 1
			     val updateN = Dyn_Array.update1
			     val subN = Dyn_Array.sub1)

  structure Byte2 = DXByteN (structure Dyn_Array = Dyn_Array
			     type byteN = FoxWord16.word
			     val n = 2
			     val updateN = update2
			     val subN = sub2)

  structure Byte4 = DXByteN (structure Dyn_Array = Dyn_Array
			     type byteN = FoxWord32.word
			     val n = 4
			     val updateN = update4
			     val subN = sub4)
 end












