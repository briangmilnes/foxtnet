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

	This is a use file for an RPC_CALL for Kerberos packets,
	and a LINEARIZEr for Kerberos packets.

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log

		iii.	RCS Log
	
$Log: use.sml,v $
Revision 1.1  1994/08/25  12:13:39  robby
Initial revision

Revision 1.2  1994/07/14  20:34:07  robby
moved stuff out of this directory

Revision 1.1  94/07/13  18:49:18  robby
Initial revision


*)
local
  val add_path=map (fn x => "app/kerberos/rpc/"^x)

  val kerberos_rpc_sig=add_path
    ["krbrealm.sig","bytearray_rpc.sig","kerberos_rpc.sig"]

  val kerberos_rpc_fun=add_path
    ["krbrealm.fun", "bytearray_rpc.fun", "kerberos_rpc.fun"]
in
  val kerberos_rpc=kerberos_rpc_sig @ kerberos_rpc_fun
end
