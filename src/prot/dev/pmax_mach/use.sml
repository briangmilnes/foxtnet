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
Revision 1.4  1995/03/07  23:53:58  esb
updated to 1.07.

Revision 1.3  1995/02/11  06:50:35  esb
added deviceUser.

Revision 1.2  1994/10/26  20:58:50  robby
Brian's changes:
Changed alpha_osf1 to pmax_mach, deleted ethdev.str from the list

Revision 1.1  1994/10/20  17:56:54  cline
Initial revision


	1.	dev, test_dev, real_dev
*)

val dev_machine =
     map (fn x => "./prot/dev/pmax_mach/" ^ x) 
         (["sys.sig", "sys.sml", "deviceUser.sig", "deviceUser.sml",
           "mach_portUser.sig", "mach_portUser.sml",
	   "ethdev.fun", "buildethdev.fun"])
