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

	A link file for the arp module.

		iii.	RCS Log
	
$Log: use.sml,v $
Revision 1.6  1994/08/02  20:29:01  esb
removed arpresolve.{sig,fun}

Revision 1.5  1993/10/27  01:19:26  esb
added arpresolve.

Revision 1.4  1993/10/12  22:34:28  esb
added time_arp.

Revision 1.3  1993/09/20  23:12:21  esb
restructured into code, test, and real.

Revision 1.2  1993/09/13  22:07:41  cline
deleted '#'s from RCS log

Revision 1.1  1993/08/24  21:20:29  esb
Initial revision


*)

val arp = map (fn x => "./prot/arp/" ^ x)
              ["arp.sig", "arp.fun", "arpeth.fun"]

val test_arp = map (fn x => "./prot/arp/" ^ x) ["arp.tst"]

val real_arp = map (fn x => "./prot/arp/" ^ x) ["arpreal.tst"]

val time_arp = map (fn x => "./prot/arp/" ^ x) []
