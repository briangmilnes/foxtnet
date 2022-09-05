(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edoardo Biagioni <Edoardo.Biagioni@cs.cmu.edu>
	Brian Milnes (Brian.Milnes@cs.cmu.edu)
        Ken Cline    (Ken.Cline@cs.cmu.edu)
	Nick Haines  (Nick.Haines@cs.cmu.edu)
	Robert Findler (Robert.Findler@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

		i.	Abstract

	A link file for Kerberos.

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	Kerberos


		iii.	RCS Log
	
$Log: use.sml,v $
Revision 1.9  1994/08/25  14:39:23  robby
updated for new directory structure

Revision 1.8  1994/08/25  14:29:44  milnes
Changed loading lists.

Revision 1.7  1994/08/08  14:53:45  danwang
Added build kerberos.fun.

Revision 1.6  1994/07/13  19:11:48  robby
updated for the new kerberos

Revision 1.5  94/07/04  21:02:15  robby
updated to the new kerberos stuff.

Revision 1.4  94/05/24  14:33:41  robby
 Ken's restructuring following a code review.

Revision 1.3  1993/12/15  15:17:53  cline
Removed #s from changelog.

Revision 1.2  1993/12/10  18:47:09  cline
*** empty log message ***

Revision 1.1  1993/10/20  14:24:34  cline
Initial revision



		1.	Kerberos
*)

use "app/kerberos/rpc/use.sml";
use "app/kerberos/linearize/use.sml";

val kerberos = kerberos_rpc@kerberos_linearize@
  (map (fn n => "./app/kerberos/" ^ n) ["kerberos.sig","kerberos.fun", 
					"buildkerberos.fun"])
