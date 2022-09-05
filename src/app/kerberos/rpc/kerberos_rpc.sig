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

	This is a specialization of the RPC_CALL signature
	to Kerbreos

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	signature Kerberos_RPC

		iii.	RCS Log
	
$Log: kerberos_rpc.sig,v $
Revision 1.1  1994/08/25  12:13:36  robby
Initial revision

Revision 1.1  1994/07/13  18:48:54  robby
Initial revision


		1.	signature Kerberos_RPC
*)
signature KERBEROS_RPC_CALL=
sig
  datatype realm=Realm of string
  datatype Kerberos_Call_Error=No_Response | Other
  include RPC_CALL
  structure K:KERBEROS_LINEARIZE
  sharing type info=control=unit
  sharing type address=realm
  sharing type error=Kerberos_Call_Error
end



