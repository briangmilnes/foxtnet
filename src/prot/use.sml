(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edoardo Biagioni (esb@cs.cmu.edu)
	Nick Haines  (Nick.Haines@cs.cmu.edu)
	Brian Milnes (Brian.Milnes@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

		i.	Abstract


		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	

		iii.	RCS Log
	
$Log: use.sml,v $
Revision 1.32  1994/11/11  18:11:32  esb
added real_icmp.

Revision 1.31  1994/10/26  14:00:51  esb
added stack_demo

Revision 1.30  1994/10/19  23:18:51  milnes
added addressing.

Revision 1.29  1994/10/10  18:28:12  milnes
Added tcpip.

Revision 1.28  1994/08/29  21:59:56  robby
added buildudptcp.fun and .sig to the protocols variable

Revision 1.27  1994/08/29  18:16:54  robby
added buildudptcp

Revision 1.26  1994/08/02  20:35:22  esb
added conn.{sig,fun,tst}.

Revision 1.25  1994/07/04  21:35:03  esb
added real_dns.

Revision 1.24  1994/06/29  19:55:59  milnes
Added test_dns.

Revision 1.23  1994/06/29  15:48:01  robby
added rpc to the protocols.

Revision 1.22  94/06/20  20:15:07  esb
replaced timeprotocol.fun with benchmark.sig and benchmark.fun.

Revision 1.21  1994/05/23  16:38:37  milnes
Added test_icmp.

Revision 1.20  1994/05/23  14:05:03  milnes
Added icmp.

Revision 1.19  1994/04/27  00:01:52  esb
removed timeprotocol.sig and prot.tim from real_proto.

Revision 1.18  94/04/20  14:51:19  milnes
Added timingtest.fun.

Revision 1.17  1994/03/29  17:44:40  milnes
Added tests.tim.

Revision 1.16  1994/02/17  17:10:27  milnes
Checked out, reinstalled old, rechecked in to repair the afs file move problems that confused rcs.

Revision 1.14  1994/01/18  17:52:33  milnes
Removed foxmachines.

Revision 1.13  1994/01/18  04:24:04  esb
added testaddress.str

Revision 1.12  1993/11/09  22:11:32  milnes
Added the build variables for protocols.

Revision 1.11  1993/10/25  17:30:56  milnes
Added udp.

Revision 1.10  1993/10/12  22:33:09  esb
added time_proto and time_protocols.

Revision 1.9  1993/10/09  23:19:23  esb
addedd arp, ip, and tcp to the real_ variable.

Revision 1.8  1993/10/09  18:25:33  esb
changed "time_" back to "real_", since some of the files included
under this name measure functionality but not timing or performance.

Revision 1.7  1993/10/08  15:47:01  milnes
Updated for timing.

Revision 1.6  1993/09/20  23:13:02  esb
restructured into code, test, and real.

Revision 1.5  1993/08/26  18:07:39  milnes
Added Arp for Edo.

Revision 1.4  1993/06/22  18:44:16  esb
fixed a minor problem

Revision 1.3  1993/06/22  17:54:53  esb
added user.fun

Revision 1.2  1993/06/11  17:47:52  esb
made paths relative

Revision 1.1  1993/06/10  23:00:46  milnes
Initial revision


		1.	prot
*)

use "prot/addressing/use.sml";
use "prot/dev/use.sml";
use "prot/eth/use.sml";
use "prot/arp/use.sml";
use "prot/ip/use.sml";
use "prot/icmp/use.sml";
use "prot/udp/use.sml";
use "prot/tcp/use.sml";
use "prot/rpc/use.sml";

val proto = ["prot/proto.sig", "prot/conn.sig", "prot/conn.fun",
	     "prot/user.sig", "prot/user.fun",
	     "prot/testaddress.str", 
             "prot/protocolnumbers.sig",
             "prot/defaultprotocolnumbers.str"]

             
val test_proto = ["prot/conn.tst"]

val real_proto = []

val time_proto = ["prot/timingtest.sig", "prot/timingtest.fun",
                  "prot/timeprotocol.sig",
		  "prot/benchmark.sig", "prot/benchmark.fun"]

val proto_post = ["prot/buildudptcp.sig","prot/buildudptcp.fun",
                  "prot/tcpipeth.sig", "prot/buildtcpipeth.fun",
		  "prot/tcpipeth.str"]

val protocols = proto @ addressing @ dev @ eth @ arp @ ip @ icmp @ udp
              @ tcp @ rpc @ proto_post

val test_protocols = test_proto @ test_dev @ test_eth @
                     test_arp @ test_ip @ test_icmp @ test_udp @ test_tcp @
		     test_rpc

val real_protocols = real_proto @ real_dev @ real_eth @
	             real_arp @ real_ip @ real_icmp @ real_udp @
		     real_tcp @ real_dns

val time_protocols = time_proto @ time_dev @ time_eth @
	             time_arp @ time_ip  @ time_udp @ time_tcp @
		     ["prot/proto.tim", "prot/tests.tim"]

val build_protocols = ["prot/dev/buildethdev.fun",
		       "prot/eth/buildeth.fun", "prot/ip/buildip.fun", 
                       "prot/udp/buildudp.fun", "prot/tcp/buildtcp.fun",
		       "prot/buildudptcp.sig","prot/buildudptcp.fun"]

val stack_demo = ["prot/simple.fun", "prot/demo.str"]
