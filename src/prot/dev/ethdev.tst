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

   A simple test functor and structure for ethdev.


		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor Test_Eth_Dev
	2.	structure Test_Eth_Dev

		iii.	RCS Log
	
$Log: ethdev.tst,v $
Revision 1.10  1995/09/18  19:30:07  esb
unusable.

Revision 1.9  1995/03/07  20:37:42  esb
updated tracing.

Revision 1.8  1995/01/14  02:30:54  esb
adapted to new filter interface.

Revision 1.7  1994/10/27  20:27:43  cline
changed Buffer.buf to Bytearray.bytearray

Revision 1.6  1994/10/20  14:36:41  cline
support for SML/NJ 105b

Revision 1.5  1994/09/12  18:20:01  milnes
Added ip_address_option.

Revision 1.4  1994/08/02  20:25:36  esb
adapted to new protocol signature.

Revision 1.3  1994/07/01  02:34:28  danwang
Moved control structures into Fox_Basis.

Revision 1.2  1994/06/16  16:45:05  danwang
Updated to use functorized Fox_Basis

Revision 1.1  1994/05/23  14:13:57  milnes
Initial revision


		1.	functor Test_Eth_Dev
*)

functor Test_Eth_Dev (structure B: FOX_BASIS): TEST_STRUCTURE =
 struct
  structure S = Build_Eth_Dev (structure B = B
			       val high_priority_filter = false
			       val debug_level = NONE)

  structure Dev = S.Eth_Dev

  fun test_makestring_address () =
       (Dev.Address.makestring () = "()")

  fun test_makestring_incoming () = 
       let fun init i = FoxWord8.intToWord (i mod 256)
	   val packet = B.Dyn_Array.init1 (50, init)
	   val dest1 = "\nTo    = 1ux00.1ux01.1ux02.1ux03.1ux04.1ux05"
	   val source1 = "\nFrom  = 1ux06.1ux07.1ux08.1ux09.1ux0a.1ux0b"
	   val proto1 = "\nproto = 1ux0c.1ux0d"
	   val data10 = "\ndata  = 1ux0e.1ux0f.1ux10.1ux11.1ux12.1ux13.1ux14."
	   val data11 = "1ux15.1ux16.1ux17.1ux18.1ux19.1ux1a.1ux1b.1ux1c."
	   val data12 = "1ux1d.1ux1e.1ux1f.1ux20.1ux21.1ux22.1ux23.1ux24."
	   val data13 = "1ux25.1ux26.1ux27.1ux28.1ux29.1ux2a.1ux2b.1ux2c."
	   val data14 = "1ux2d.1ux2e.1ux2f.1ux30.1ux31\n"
	   val output1 = dest1 ^ source1 ^ proto1 ^
                         data10 ^ data11 ^ data12 ^ data13 ^ data14
	   val dest2 = "\nTo    = 1ux00.1ux01.1ux02.1ux03.1ux04.1ux05"
	   val source2 = "\nFrom  = 1ux06.1ux07.1ux08.1ux09.1ux0a.1ux0b"
	   val proto2 = "\nproto = 1ux0c.1ux0d"
	   val data20 = "\ndata  = 1ux0e.1ux0f.1ux10.1ux11.1ux12.1ux13.1ux14."
	   val data21 = "1ux15.1ux16.1ux17.1ux18.1ux19.1ux1a.1ux1b.1ux1c."
	   val data22 = "1ux1d.1ux1e.1ux1f.1ux20.1ux21...\n"
	   val output2 = dest2 ^ source2 ^ proto2 ^ data20 ^ data21 ^ data22
       in Dev.makestring_incoming (packet, NONE) = output1 andalso
	  Dev.makestring_incoming (packet, SOME 20) = output2
       end

  fun test_makestring_outgoing () = 
       let fun init i = FoxWord8.intToWord (i mod 256)
	   val packet = B.Dyn_Array.init1 (50, init)
	   val dest1 = "\nTo    = 1ux00.1ux01.1ux02.1ux03.1ux04.1ux05"
	   val source1 = "\nFrom  = 1ux06.1ux07.1ux08.1ux09.1ux0a.1ux0b"
	   val proto1 = "\nproto = 1ux0c.1ux0d"
	   val data10 = "\ndata  = 1ux0e.1ux0f.1ux10.1ux11.1ux12.1ux13.1ux14."
	   val data11 = "1ux15.1ux16.1ux17.1ux18.1ux19.1ux1a.1ux1b.1ux1c."
	   val data12 = "1ux1d.1ux1e.1ux1f.1ux20.1ux21.1ux22.1ux23.1ux24."
	   val data13 = "1ux25.1ux26.1ux27.1ux28.1ux29.1ux2a.1ux2b.1ux2c."
	   val data14 = "1ux2d.1ux2e.1ux2f.1ux30.1ux31\n"
	   val output1 = dest1 ^ source1 ^ proto1 ^
                         data10 ^ data11 ^ data12 ^ data13 ^ data14
	   val dest2 = "\nTo    = 1ux00.1ux01.1ux02.1ux03.1ux04.1ux05"
	   val source2 = "\nFrom  = 1ux06.1ux07.1ux08.1ux09.1ux0a.1ux0b"
	   val proto2 = "\nproto = 1ux0c.1ux0d"
	   val data20 = "\ndata  = 1ux0e.1ux0f.1ux10.1ux11.1ux12.1ux13.1ux14."
	   val data21 = "1ux15.1ux16.1ux17.1ux18.1ux19.1ux1a.1ux1b.1ux1c."
	   val data22 = "1ux1d.1ux1e.1ux1f.1ux20.1ux21...\n"
	   val output2 = dest2 ^ source2 ^ proto2 ^ data20 ^ data21 ^ data22
       in Dev.makestring_outgoing (packet, NONE) = output1 andalso
	  Dev.makestring_outgoing (packet, SOME 20) = output2
       end

  fun run () =
   (B.Test.tests ("Ethernet Device", 3,
     (fn () => 
      (B.Test.test ("makestring_address", test_makestring_address);
       B.Test.test ("makestring_incoming", test_makestring_incoming);
       B.Test.test ("makestring_outgoing", test_makestring_outgoing)))))

  val _ = if ! B.Debug.do_tests then run () else ()

 end



(*
		2.	structure Test_Eth_Dev
*)

structure Test_Eth_Dev = Test_Eth_Dev (structure B = Fox_Basis)








