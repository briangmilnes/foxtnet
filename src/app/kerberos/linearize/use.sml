(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edoardo Biagioni (Edoardo.Biagioni@cs.cmu.edu)
	Brian Milnes (Brian.Milnes@cs.cmu.edu)
	Ken Cline (Kenneth.Cline@cs.cmu.edu)
	Robert Findler (Robert.Findler@cs.cmu.edu)
        Nick Haines (Nick.Haines@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

		i.	Abstract

	This is a use file for a LINEARIZEr for Kerberos packets.

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log

		iii.	RCS Log
	
$Log: use.sml,v $
Revision 1.2  1994/08/25  11:04:20  robby
changed location

Revision 1.1  1994/08/25  10:24:42  robby
Initial revision

Revision 1.3  1994/07/15  18:43:40  robby
ommitted a file by accedent.

Revision 1.2  94/07/14  22:58:29  robby
swapped around some files

Revision 1.1  94/07/14  21:14:04  robby
Initial revision

Revision 1.1  94/07/14  20:29:55  robby
Initial revision


*)
val kerberos_linearize=map (fn x => "app/kerberos/linearize/"^x)
  ["kerberos_error.sig","kerberos_misc.sig",
   "kerberos_tag.sig","kerberos_linearize.sig",
   "kerberos_error.fun","kerberos_misc.fun",
   "kerberos_tag.fun","kerberos_linearize.fun"]
