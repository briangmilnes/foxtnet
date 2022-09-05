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



		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log

		iii.	RCS Log
	
$Log: use.sml,v $
Revision 1.5  1994/08/25  23:47:10  robby
added buildlinearize.fun and .sig

Revision 1.4  1994/08/25  16:26:11  robby
removed kerberos and des linearize

Revision 1.3  1994/08/25  16:22:39  milnes
added kerberos linearize and des.

Revision 1.2  1994/07/14  22:58:12  robby
swapped around somefiles

Revision 1.1  94/07/14  21:13:49  robby
Initial revision


*)

val linearize=map (fn x => "extern/linearize/"^x)
    ["swap.sig","linearize.sig","ubyte.sig","int.sig","pair.sig",
     "bytearray.sig","string.sig",
     "swap.fun","bytearray.fun","cstring.fun","pair.fun",
     "ubyte.fun","int.fun","buildlinearize.sig","buildlinearize.fun"]


