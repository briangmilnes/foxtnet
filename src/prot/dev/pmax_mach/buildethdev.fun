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
Revision 1.7  1995/04/12  16:40:22  cstone
Turned off high_priority_filter & removed this from functor args.

Revision 1.6  1995/03/07  23:53:58  esb
updated to 1.07.

Revision 1.5  1995/01/17  20:59:27  esb
fixed a bug.

Revision 1.4  1995/01/16  23:48:50  esb
removed obsolete functor parameters.

Revision 1.3  1995/01/14  02:32:39  esb
adapted to new filter interface.

Revision 1.2  1994/11/10  16:06:43  milnes
Updated for the tcpipeth/addressing scheme.

Revision 1.1  1994/10/20  17:56:54  cline
Initial revision

Revision 1.13  1994/09/12  18:19:15  milnes
Added ip_address_option.

Revision 1.12  1994/08/02  20:25:36  esb
adapted to new protocol signature.

Revision 1.11  1994/07/01  02:34:01  danwang
Moved control structures into Fox_Basis.

Revision 1.10  1994/06/16  16:45:05  danwang
Updated to use functorized Fox_Basis

Revision 1.9  1994/06/07  16:31:25  robby
Added a signature

Revision 1.8  94/03/07  13:12:18  esb
removed some functor parameters that are no longer needed.

Revision 1.7  94/01/28  23:42:44  esb
fixed a major bug by setting reuse_buffer to false. Do not change this.

Revision 1.6  1993/12/20  16:31:14  cline
val high_priority_filter = high_priority_filter

Revision 1.5  1993/12/20  16:26:33  cline
Added high_priority_filter functor parameter.

Revision 1.4  1993/11/11  05:02:09  esb
functor parameter changes.

Revision 1.3  1993/11/10  17:32:18  milnes
Nothing changed.

Revision 1.2  1993/10/26  20:06:41  esb
added the new functor parameters needed by ethdev.

Revision 1.1  1993/10/25  17:28:51  milnes
Initial revision


		1.	functor BuildEthDev
*)


functor Build_Eth_Dev (structure B: FOX_BASIS
		       val debug_level: int ref option): BUILD_ETH_DEV =
 struct
  local
   structure Device = Device (structure B = B
			      structure MachIPC = MachIPC
			      structure Word = Word
			      structure Mig_Base = Mig_Base)

   val task_by_pid = System.Unsafe.CInterface.c_function "FoxNet" "task_by_pid"

  in

   structure Eth_Dev = 
       Ethernet_Device (val device_name = "SE0"
			structure Device = Device
			structure Local_Mach = MachIPC
			structure Mach_Port = Mach_Port
			structure Mig_Base = Mig_Base
			structure Filter = B.Filter
			val task_by_pid = task_by_pid
			val high_priority_filter = false
			structure B = B
			val debug_level = debug_level)
  end (* local *)
 end (* struct *)





