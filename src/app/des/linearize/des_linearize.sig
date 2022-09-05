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

	This is a signature that specializes the LINEARIZE signature
	to DES keys.

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	signature DES_KEY_LINEARIZE

		iii.	RCS Log
	
$Log: des_linearize.sig,v $
Revision 1.1  1994/08/25  12:15:18  robby
Initial revision

Revision 1.1  1994/07/14  20:30:44  robby
Initial revision

Revision 1.1  94/07/13  18:44:16  robby
Initial revision


	1.	signature DES_KEY_LINEARIZE
*)
signature DES_KEY_LINEARIZE=
sig
  include LINEARIZE

  structure Des:DES'
  sharing type T=Des.key
end
