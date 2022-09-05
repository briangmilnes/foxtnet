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

	A functor to build ethernet on top of ethdev.

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor BuildEth

		iii.	RCS Log
	
$Log: buildeth.fun,v $
Revision 1.16  1995/11/12  11:29:27  esb
removed some functor arguments.

Revision 1.15  1995/11/08  23:10:27  cline
*** empty log message ***

Revision 1.14  1995/06/20  16:56:38  esb
adapted to new protocol signature.

Revision 1.13  1995/03/07  20:32:16  esb
updated tracing.

Revision 1.12  1994/11/10  16:06:43  milnes
Updated for the tcpipeth/addressing scheme.

Revision 1.11  1994/08/02  20:26:48  esb
adapted to new protocol signature.

Revision 1.10  1994/07/01  02:25:03  danwang
Moved control structures into Fox_Basis.

Revision 1.9  1994/06/16  16:37:34  danwang
Updated to use functorized Fox_Basis

Revision 1.8  1994/06/07  16:26:52  robby
changed name of signature

Revision 1.7  94/06/05  19:55:44  robby
Added a signature

Revision 1.6  94/01/18  04:17:53  esb
now functor takes a DEVICE_PROTOCOL parameter.

Revision 1.5  1994/01/06  16:28:44  cline
Aligned program text.

Revision 1.4  1993/12/20  16:31:50  cline
added high_priority_filter

Revision 1.3  1993/11/11  04:53:31  esb
changed functor parameters.

Revision 1.2  1993/11/09  22:06:24  milnes
Checked out to change do_prints.

Revision 1.1  1993/10/25  17:32:30  milnes
Initial revision


		1.	functor BuildEth
*)

functor Build_Eth (structure Device: DEVICE_PROTOCOL
		     sharing type Device.Incoming.T = Device.Outgoing.T
		   structure B: FOX_BASIS
                   val debug_level: int ref option): BUILDETH =
 struct

  structure Device = Device
  structure Eth = Ethernet (structure Device = Device
                            structure B = B
			    val debug_level = debug_level)
 end

