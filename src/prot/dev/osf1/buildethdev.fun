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

	A functor to build an EthDev.


		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor BuildEthDev

		iii.	RCS Log
	
$Log: buildethdev.fun,v $
Revision 1.9  1995/11/08  23:10:27  cline
*** empty log message ***

Revision 1.8  1995/09/18  19:29:17  esb
first running version.

Revision 1.7  1995/03/07  23:55:22  esb
updated tracing.

Revision 1.6  1995/01/16  23:48:37  esb
removed obsolete functor parameters.

Revision 1.5  1995/01/14  02:31:20  esb
adapted to new filter interface.

Revision 1.4  1994/12/21  20:36:49  milnes
Updated for duplicate addressing.

Revision 1.3  1994/11/22  13:58:38  milnes
Removed addressing functor arguments.

Revision 1.2  1994/11/10  16:06:43  milnes
Updated for the tcpipeth/addressing scheme.

Revision 1.1  1994/10/20  17:57:49  cline
Initial revision


		1.	functor BuildEthDev
*)

functor Build_Eth_Dev (structure B: FOX_BASIS
		       val debug_level: int ref option): BUILD_ETH_DEV =
 struct
  local 
   structure Packet_Filter = Packet_Filter (structure B = B
					    val debug_level = debug_level)
  in
   structure Raw = 
    Ethernet_Device (structure Packet_Filter = Packet_Filter
                     structure B = B
		     val debug_level = debug_level)

   structure Dev = Device_To_Protocol (structure Device = Raw
				       structure B = B
				       val debug_level = debug_level)
  end (* local *)
 end (* struct *)





