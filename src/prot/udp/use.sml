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

	A link file for the UDP protocol.

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	


		iii.	RCS Log
	
$Log: use.sml,v $
Revision 1.6  1994/06/07  16:36:54  robby
Added buildudp.sig

Revision 1.5  94/01/17  13:22:19  cline
added uudp.tst

Revision 1.4  1993/10/25  17:35:48  milnes
Added buildudp.

Revision 1.3  1993/10/18  19:44:38  cline
define udp, test_udp, real_udp, and time_udp

Revision 1.2  1993/06/11  17:25:26  esb
made file names relative to root of source directory

Revision 1.1  1993/06/10  23:10:21  milnes
Initial revision


		1.	
*)


local
  fun prefix l = map (fn x => "./prot/udp/" ^ x) l
in
  val udp      = prefix ["udp.sig", "udp.fun", "buildudp.sig", "buildudp.fun"]

  val test_udp = prefix ["udp.tst", "uudp.tst"]

  val real_udp = prefix []

  val time_udp = prefix ["udp.tim"]
end
