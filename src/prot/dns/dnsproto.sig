(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edoardo Biagioni (Edoardo.Biagioni@cs.cmu.edu)
	Brian Milnes (Brian.Milnes@cs.cmu.edu)
	Ken Cline (Kenneth.Cline@cs.cmu.edu)
        Nick Haines (Nick.Haines@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

		i.	Abstract

	Low level DNS Protocol implementation


		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	signature DNS_PROTOCOL

		iii.	RCS Log
	
$Log: dnsproto.sig,v $
Revision 1.2  1996/03/04  21:29:47  esb
added structure Message.

Revision 1.1  1996/01/16  22:00:05  cline
Initial revision

Revision 1.1  1994/06/29  19:29:56  milnes
Initial revision

	1.	signature DNS_PROTOCOL
*)

signature DNS_PROTOCOL =
 sig
  include PROTOCOL

  structure Message: DNS_EXTERN

  sharing type Message.message = Incoming.T = Outgoing.T
      and type Message.internet_address = Address.T
 end

