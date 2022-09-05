(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edoardo Biagioni (Edoardo.Biagioni@cs.cmu.edu)
	Brian Milnes (Brian.Milnes@cs.cmu.edu)
	Ken Cline (Kenneth.Cline@cs.cmu.edu)
        Nick Haines (Nick.Haines@cs.cmu.edu)
	Robert Findler (Robert.Findler@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

		i.	Abstract

	This is a specialization of the LINEARIZE signature
	to int.

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	signature INT_LINEARIZE

		iii.	RCS Log
	
$Log: int.sig,v $
Revision 1.1  1994/07/14  20:28:41  robby
Initial revision

Revision 1.1  94/07/13  18:45:31  robby
Initial revision


		1.	signature INT_LINEARIZE
*)

signature INT_LINEARIZE =
sig
  include LINEARIZE
  sharing type T=int
end
