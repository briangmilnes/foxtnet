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
	Pittsburgh, Pa 15213-3891

		i.	Abstract



		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log

		iii.	RCS Log
	
$Log: use.sml,v $
Revision 1.4  1995/02/09  19:48:44  esb
added xdr.

Revision 1.3  1994/08/25  23:20:42  milnes
Added dynarray and linearize.

Revision 1.2  1994/07/14  22:57:36  robby
fixed a typo

Revision 1.1  94/07/14  21:13:41  robby
Initial revision

*)

use "extern/linearize/use.sml";
use "extern/dynarray/use.sml";
use "extern/xdr/use.sml";

val extern = ["extern/extern.sig"] @ linearize @ dxs @ xdr

val test_extern = test_xdr


