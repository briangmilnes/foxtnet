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

	A link file for the trivial ip module.


		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	ip


		iii.	RCS Log
	
$Log: use.sml,v $
Revision 1.24  1994/10/26  10:05:32  esb
added pseudoip.fun.

Revision 1.23  1994/08/15  19:54:09  milnes
Added icmp.sig and emptyicmp.fun.

Revision 1.22  1994/08/02  20:31:33  esb
removed ipaux.{sig,fun}, ipresolve.sig, and ipeth.fun.

Revision 1.21  1994/06/15  20:47:40  milnes
Installed subnet routing.

Revision 1.20  1994/06/07  16:33:24  robby
 Added a signature

Revision 1.19  94/05/03  20:55:11  milnes
Added ip option handling.

Revision 1.18  1994/03/09  03:07:59  esb
added ipfrag.tim.

Revision 1.17  94/03/03  21:36:06  esb
removed ip.tim from real_ip.

Revision 1.16  94/01/13  15:11:52  cline
Added IP fragmentation.

Revision 1.15  1993/12/17  02:34:38  esb
added ipresolve.sig and ipeth.fun.

Revision 1.14  1993/11/10  18:09:07  esb
added ipresolve.sig, ipeth.fun, and ipreal.tst

Revision 1.13  1993/10/25  17:34:12  milnes
Added buildip.

Revision 1.12  1993/10/19  18:48:38  milnes
Added the file ip.tim to the time_ip load variable.

Revision 1.11  1993/10/15  16:00:24  cline
added ipaux.{sig,fun}

Revision 1.10  1993/10/12  22:34:40  esb
added time_ip.

Revision 1.9  1993/09/20  23:11:58  esb
restructured into code, test, and real.

Revision 1.8  1993/07/16  19:09:07  esb
adapted to new module structure of this directory.

Revision 1.7  1993/06/29  19:53:40  esb
added uip.tst

Revision 1.6  1993/06/18  18:07:22  esb
restored the files that had been temporarily commented out

Revision 1.5  1993/06/18  14:38:06  esb
fixed a syntax error

Revision 1.4  1993/06/18  14:33:14  esb
added ipeth.tst and temporarily commented out ipcheck.fun, iptop.fun, ip.tst

Revision 1.3  1993/06/14  17:21:05  esb
renamed files from ".sml" to ".fun", ".tst"

Revision 1.2  1993/06/11  17:25:26  esb
made file names relative to root of source directory

Revision 1.1  1993/06/10  23:08:40  milnes
Initial revision


		1.	ip
*)

local
  fun prefix l = map (fn x => "./prot/ip/" ^ x) l
in
  val ip = prefix ["ipoptions.sig", "icmp.sig", "ip.sig",
		   "ipfrag.sig", "iproute.sig",
		   "ipmux.sig", "ipfrag.fun",
		   "ipoptions.fun", "iproute.fun", "emptyicmp.fun",
		   "ip.fun", "iploop.fun", "pseudoip.fun",
		   "buildip.sig",   "buildip.fun"]

  val test_ip = prefix ["ip.tst", "uip.tst", "ipoptions.tst"]

  val real_ip = prefix ["ipreal.tst"]

  val time_ip = prefix ["ip.tim"]
end
