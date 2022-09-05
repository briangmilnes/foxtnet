(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edoardo Biagioni (Edoardo.Biagioni@cs.cmu.edu)
	Brian Milnes (Brian.Milnes@cs.cmu.edu)
	Ken Cline (Kenneth.Cline@cs.cmu.edu)
        Nick Haines (Nick.Haines@cs.cmu.edu)
	Robert.Findler (Robert.Findler@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

		i.	Abstract



		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	

		iii.	RCS Log

$Log: use.sml,v $
Revision 1.8  1994/12/01  18:46:32  esb
removed extern.sig, which is defined in ..../extern.

Revision 1.7  1994/08/25  16:20:46  robby
removed kerberos subdirectory

Revision 1.6  1994/07/14  21:23:07  robby
added kerberos subdirectory

Revision 1.5  94/07/07  17:00:28  robby
added extern.sig

Revision 1.4  94/07/04  21:36:08  esb
added real_rpc and real_dns.

Revision 1.3  1994/06/29  19:55:46  milnes
Added test_dns.

Revision 1.2  1994/06/29  19:48:10  milnes
Added dns.

Revision 1.1  1994/06/29  14:14:03  robby
Initial revision


		1.	...
*)

use "prot/rpc/dns/use.sml";

val rpc = (map (fn x=>"./prot/rpc/"^x) ["rpc.sig"]) @ dns 
  
val test_rpc = test_dns

val real_rpc = real_dns
