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

	This is a signature for marshalling strings

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	signature STRING_LINEARIZE

		iii.	RCS Log
	
$Log: string.sig,v $
Revision 1.1  1994/07/14  20:28:48  robby
Initial revision

Revision 1.1  94/07/13  18:49:09  robby
Initial revision


		1.	signature STRING_LINEARIZE
*)

signature STRING_LINEARIZE =
sig
  include LINEARIZE
  sharing type T=string
end
