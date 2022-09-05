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

	This is the signature for an RPC_CALL, specialized
	to bytearrays.

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	signature BYTEARRAY_RPC_CALL

		iii.	RCS Log
	
$Log: bytearray_rpc.sig,v $
Revision 1.1  1994/08/25  12:13:35  robby
Initial revision

Revision 1.1  1994/07/13  18:44:10  robby
Initial revision


		1.	signature BYTEARRAY_RPC_CALL
*)
signature BYTEARRAY_RPC_CALL=
sig

  include RPC_CALL
  datatype ByteArray_Call_Error=No_Response | Other
  sharing type error=ByteArray_Call_Error
end



