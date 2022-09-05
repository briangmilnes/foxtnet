(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edoardo Biagioni (esb@cs.cmu.edu)
	Nick Haines  (Nick.Haines@cs.cmu.edu)
	Brian Milnes (Brian.Milnes@cs.cmu.edu)
        Ken Cline    (Ken.Cline@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

		i.	Abstract


		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	Applications

		iii.	RCS Log
	
$Log: use.sml,v $
Revision 1.13  1996/05/14  21:16:13  esb
added inetd

Revision 1.12  1994/08/29  20:39:15  robby
readded gopher

Revision 1.11  1994/08/29  10:53:05  cokasaki
added ftp

Revision 1.10  94/08/28  21:58:39  milnes
Removed gopher.

Revision 1.9  1994/08/28  18:57:06  robby
added finger

Revision 1.8  1994/08/28  18:55:46  milnes
Added kerberos.

Revision 1.7  1994/06/14  23:57:06  robby
added des to appl

Revision 1.6  94/05/10  13:10:24  milnes
Added use for ping.

Revision 1.5  1994/05/10  13:06:50  milnes
Removed ping as it is in a subdirectory.

Revision 1.4  1994/05/04  19:18:28  milnes
Added val app for loading applications (ping).

Revision 1.3  1993/12/15  15:11:00  cline
removed #s from changelog, fixed comment.

Revision 1.2  1993/12/15  13:37:16  cline
added des

Revision 1.1  1993/10/20  14:17:11  cline
Initial revision


		1.	Applications
*)

use "app/ping/use.sml";
use "app/inetd/use.sml";
use "app/des/use.sml";
use "app/kerberos/use.sml";
use "app/zephyr/use.sml";
use "app/gopher/use.sml";
use "app/finger/use.sml";
use "app/ftp/use.sml";

(* App is a standard SML/NJ maping function, that returns
 unit instead of a result list, so name the list appl. *)

val appl = ping @ des @ kerberos
