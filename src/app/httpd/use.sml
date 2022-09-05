(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edoardo Biagioni (Edoardo.Biagioni@cs.cmu.edu)
	Brian Milnes (Brian.Milnes@cs.cmu.edu)
	Ken Cline (Kenneth.Cline@cs.cmu.edu)
        Chris Stone (Christopher.Stone@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15213-3891

		i.	Abstract

                A use file for the http server (httpd)

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	httpd files

		iii.	RCS Log
	
$Log: use.sml,v $
# Revision 1.3  1995/03/08  20:31:31  cstone
# Added foxnetcpipeth.str, boast.sml, tcpstack.sig
#
# Revision 1.2  1995/01/26  17:16:55  cstone
# Fixed unterminated strings.
#
# Revision 1.1  1995/01/25  22:49:29  cstone
# Initial revision
#

		1.	httpd files
*)

val httpd = map (fn s => "./app/httpd/" ^ s)
            ["tcpstack.sig", "makeserver.sig", "makeserver.fun",
             "foxnetcpipeth.str", "httpd.sig", "httpd.fun", "boast.sml",
             "httpd.str"]




