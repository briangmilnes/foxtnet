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

	A link file for the ethernet driver module, including the basic ethernet
    types.

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	dev, test_dev, real_dev

		iii.	RCS Log
	
$Log: use.sml,v $
Revision 1.21  1995/02/11  06:50:18  esb
removed deviceUser.

Revision 1.20  1994/11/10  16:06:43  milnes
Updated for the tcpipeth/addressing scheme.

Revision 1.19  1994/10/27  18:08:13  milnes
Added a ./.

Revision 1.18  1994/10/27  16:08:02  milnes
Rearranged the load ordering.

Revision 1.17  1994/10/20  17:58:30  cline
*** empty log message ***

Revision 1.16  1994/08/12  19:14:34  esb
replaced ethdev.tim with devreal.tst.

Revision 1.15  1994/06/07  16:32:03  robby
Added a signature

Revision 1.14  94/05/23  14:11:41  milnes
Added ethdev.tsts.

Revision 1.13  1994/01/17  23:15:26  esb
added buildsim.fun.

Revision 1.12  1994/01/17  19:51:04  milnes
Changes for ip fragmentation.

Revision 1.11  93/11/04  14:02:31  esb
removed receive_thread.

Revision 1.10  1993/10/25  17:27:15  milnes
Added buildethdev.fun.

Revision 1.9  1993/10/12  22:33:51  esb
added time_dev.

Revision 1.8  1993/10/09  18:26:36  esb
changed "time_" back to "real_", since some of the files included
under this name measure functionality but not timing or performance.

Revision 1.7  1993/10/08  15:45:50  milnes
Changed for time_all.

Revision 1.6  1993/09/20  23:12:14  esb
restructured into code, test, and real.

Revision 1.5  1993/08/27  20:38:06  milnes
No real changes.

Revision 1.4  1993/06/29  19:50:34  esb
put ethdev.tim in a separate group

Revision 1.3  1993/06/23  15:54:12  milnes
Made this use ethdev.fun.

Revision 1.2  93/06/11  17:25:26  esb
made file names relative to root of source directory

Revision 1.1  1993/06/10  23:05:01  milnes
Initial revision


	1.	dev, test_dev, real_dev
*)

use "./prot/dev/@sys/use.sml";

val dev = ["prot/dev/dev.sig", "prot/dev/buildethdev.sig"] @
           dev_machine @
          map (fn x => "./prot/dev/" ^ x) 
           (["wire.sig", "wire.fun","sim.fun", "buildsim.sig", "buildsim.fun"])

val test_dev = ["./prot/dev/sim.tst", "./prot/dev/ethdev.tst"]

val real_dev = ["./prot/dev/devreal.tst"]

val time_dev = []
