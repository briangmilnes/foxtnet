(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Brian Milnes (Brian.Milnes@cs.cmu.edu)
	Ken Cline <\Kenneth.Cline@cs.cmu.edu>
	Edoardo Biagioni <Edoardo.Biagioni@cs.cmu.edu>
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

		i.	Abstract

	A signature for UDP.


		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	signature UDP


		iii.	RCS Log
	
$Log: udp.sig,v $
Revision 1.16  1997/11/19  13:50:11  cline
109.32 compatibility

Revision 1.15  96/07/22  20:47:42  cline
*** empty log message ***

Revision 1.14  1996/01/15  18:00:49  cline
removed FoxWord

Revision 1.13  1995/09/15  16:40:39  cline
work around for representation analysis bug

Revision 1.12  1995/06/20  17:07:42  esb
adapted to new protocol signature.

Revision 1.11  1994/10/27  19:14:51  esb
added start_passive_port.

Revision 1.10  1994/08/16  00:45:52  esb
stub implementation, does not pass tests.

Revision 1.9  1994/06/23  14:52:53  danwang
Changed Byte#.ubytes to ubyte#.

Revision 1.8  1994/06/20  16:12:02  esb
fixed address_pattern, which was indeterminate.

Revision 1.7  1994/06/16  16:59:54  danwang
Updated for functorized Fox_Basis

Revision 1.6  1994/06/07  17:38:53  milnes
 The signature was missing control.

Revision 1.5  1994/01/17  19:51:04  milnes
Changes for ip fragmentation.

Revision 1.4  93/12/04  20:57:22  esb
Improved comments.

Revision 1.3  1993/10/25  19:34:42  cline
removed .U from Byte[421].U

Revision 1.2  1993/10/18  20:02:44  cline
completely reworked

Revision 1.1  93/06/10  23:10:21  milnes
Initial revision


		1.	signature UDP
*)

signature UDP_PROTOCOL =
 sig
  include TRANSPORT_PROTOCOL

  datatype udp_session_extension =
    Session_Extension of {packets_sent: unit -> Word64.word,
			  packets_received: unit -> Word64.word,
			  failed_sends: unit -> Word64.word,
			  packets_rejected: unit -> Word64.word}

  sharing type session_extension = udp_session_extension

 end (* sig *)
 where type connection_extension = unit
   and type additional_listen_extension = unit
