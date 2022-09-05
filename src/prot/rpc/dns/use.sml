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

       A use file for DNS.

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	val dns

		iii.	RCS Log
	
$Log: use.sml,v $
Revision 1.4  1995/03/07  23:50:13  esb
got rid of various build functors and structures.

Revision 1.3  1994/09/22  22:08:08  milnes
Removed the tcp dns stuff, as it is not used.

Revision 1.2  1994/07/04  20:27:33  esb
replaced .lnk with .str, changed test file to be in real_dns.

Revision 1.1  1994/06/29  19:29:56  milnes
Initial revision


		1.	val dns
*)

val dns = ["./prot/rpc/dns/dns.sig", "./prot/rpc/dns/dns.fun"]
           
val test_dns = []

val real_dns = ["./prot/rpc/dns/dns.tst"]





