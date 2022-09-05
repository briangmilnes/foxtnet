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

	A use file that defines val icmp.

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	val icmp.

		iii.	RCS Log
	
$Log: use.sml,v $
Revision 1.7  1994/11/11  18:12:03  esb
put icmpecho.str into real_icmp.

Revision 1.6  1994/09/14  15:24:50  milnes
Added iplocalstatus test.

Revision 1.5  1994/09/12  18:29:56  milnes
Added buildipicmp.fun.

Revision 1.4  1994/08/28  21:47:35  milnes
Added default gateways.

Revision 1.3  1994/08/15  19:53:50  milnes
Removed icmp.sig, ip loads it (from ip) now.

Revision 1.2  1994/06/05  18:46:38  milnes
Added icmpecho.str.

Revision 1.1  1994/05/23  14:09:55  milnes
Initial revision


		1.	val icmp
*)

val icmp = ["./prot/icmp/icmp.fun", "./prot/icmp/buildicmp.fun", 
	    "./prot/icmp/ipicmp.fun", "./prot/icmp/buildipicmp.sig",
            "./prot/icmp/buildipicmp.fun"]

val test_icmp = ["./prot/icmp/icmp.tst",
                 "./prot/icmp/ipicmp.tst",
                 "./prot/icmp/addressmask.tst",
                 "./prot/icmp/iplocalstatus.tst"]

val real_icmp = ["./prot/icmp/icmpecho.str"]
