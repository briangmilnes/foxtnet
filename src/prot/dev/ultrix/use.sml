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
Revision 1.3  1996/04/22  19:11:00  cline
gratuitiious change from ULTRIX to ultrix

Revision 1.2  1996/04/18  21:29:10  cline
*** empty log message ***

Revision 1.1  1994/10/20  17:57:49  cline
Initial revision


	1.	dev, test_dev, real_dev
*)

val dev_machine =
     map (fn x => "./prot/dev/ultrix/" ^ x) 
         (["packetfilter.sig", "packetfilter.fun", "ethdev.fun",
	   "buildethdev.fun"])
