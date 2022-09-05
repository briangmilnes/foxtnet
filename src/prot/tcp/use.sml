(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edoardo Biagioni (esb@cs.cmu.edu)
	Brian Milnes (Brian.Milnes@cs.cmu.edu)
	Ken Cline (Ken.Cline+@cs.cmu.edu)
	Nick Haines  (Nick.Haines@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

		i.	Abstract

	A link file for the trivial tcp module.


		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	


		iii.	RCS Log
	
$Log: use.sml,v $
Revision 1.18  1995/01/22  15:50:22  esb
added tcpserve.tst

Revision 1.17  1994/06/07  16:34:29  robby
 Added a signature

Revision 1.16  94/03/25  16:45:33  esb
added tcpping.tst.

Revision 1.15  94/03/03  21:50:15  esb
added tcpeth.tst.

Revision 1.14  94/02/17  01:10:45  esb
added tcplog.{sig,fun} and tcpreal.tst.

Revision 1.13  94/01/19  20:48:02  esb
added tcpresend.{sig,fun}.

Revision 1.12  1994/01/10  00:03:34  esb
fixed a minor bug.

Revision 1.11  1994/01/09  03:28:30  esb
switched over to the new version of TCP.

Revision 1.10  1993/11/09  22:19:03  milnes
Added buildtcp.

Revision 1.9  1993/11/04  15:03:12  esb
adjusted the experimental new_tcp variable.

Revision 1.8  1993/11/04  14:57:47  milnes
Added tcp.tim.

Revision 1.7  1993/11/03  19:06:46  esb
temporarily added some files.

Revision 1.6  1993/10/22  13:41:29  esb
removed tcpaux.sig and iptcp.fun

Revision 1.5  1993/10/13  16:59:49  esb
added time_tcp.

Revision 1.4  1993/09/20  23:12:09  esb
restructured into code, test, and real.

Revision 1.3  1993/08/13  14:17:44  esb
major revision

Revision 1.2  1993/06/11  17:25:26  esb
made file names relative to root of source directory

Revision 1.1  1993/06/10  23:11:58  milnes
Initial revision


		1.	tcp
*)

val tcp = map (fn x => "./prot/tcp/" ^ x)
		["tcplog.sig", "tcp.sig", "tcplog.fun",
		 "tcptcb.sig", "tcptcb.fun",
		 "tcpresend.sig", "tcpresend.fun",
		 "tcpstate.sig", "tcpstate.fun",
		 "tcpsend.sig", "tcpsend.fun",
		 "tcpreceive.sig", "tcpreceive.fun",
		 "tcpaction.sig", "tcpaction.fun",
		 "tcpmain.fun", 
		 "buildtcp.sig", "buildtcp.fun"]

val test_tcp = map (fn x => "./prot/tcp/" ^ x)
		["tcpstate.tst", "tcpsend.tst",
		 "tcpreceive.tst", "tcpaction.tst",
		 "tcp.tst", "utcp.tst", "tcpping.tst"]

val real_tcp = map (fn x => "./prot/tcp/" ^ x)
                ["tcpecho.tst", "tcpreal.tst", "tcpeth.tst", "tcpserve.tst"]

val time_tcp = map (fn x => "./prot/tcp/" ^ x) ["tcp.tim"]
