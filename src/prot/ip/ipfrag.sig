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


	IP packet fragmentation and reassembly support.

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	signature IP_FRAGMENT

		iii.	RCS Log
	
$Log: ipfrag.sig,v $
Revision 1.11  1996/01/19  23:02:29  esb
adapted to the new wordarray signature.

Revision 1.10  1995/08/29  14:14:54  esb
version that includes IP and ICMP.

Revision 1.9  1995/08/08  18:22:07  esb
changed to support ICMP.

Revision 1.8  1995/06/23  19:55:30  esb
first version of IP that compiles with the new PROTOCOL signature.

Revision 1.7  1995/06/20  17:03:51  esb
major change, made state explicit.

Revision 1.6  1994/10/11  15:43:04  cline
Upgraded to work with SML/NJ 1.05b

Revision 1.5  1994/08/02  20:32:38  esb
adapted to new protocol signature.

Revision 1.4  1994/06/16  16:39:20  danwang
Updated to use functorized Fox_Basis

Revision 1.3  1994/02/21  00:00:50  esb
changed the interface to fragment.

Revision 1.2  94/01/17  19:51:04  milnes
Changes for ip fragmentation.

Revision 1.1  94/01/14  20:07:52  cline
Initial revision



		1.	signature IP_FRAGMENT
*)

signature IP_FRAGMENT =
 sig
  type ip_header
  type incoming
  type outgoing
  type T			(* explicit state, managed by caller *)

  (* Before calling fragment, check that the packet is too
     large to send as is, and that its Do-Not-Fragment flag
     is clear.

     Fragment takes the following arguments:
       packet: the packet to be fragmented, with no header.
       header: the header to prepend to the packet.
       max_size, min_size: size of the largest and smallest
           IP packets (size includes headers) we can send.

     Fragment returns a list of packets to send, with the correct
     header prepended to each. *)

  val fragment: {packet: outgoing, header: ip_header,
		 max_size: Word.word, min_size: Word.word}
              -> (outgoing * ip_header * Word.word (* header size *) ) list

  (* Reassemble should be called whenever a packet fragment
     (indicated by the More-Fragments flag, or nonzero
     Fragment-Offset field) arrives.

     Reeassemble returns SOME <packet> following successful reassembly,
     and NONE when more fragments are needed or following an error.

     Reassemble uses some state, which must be managed by the caller.
     State is created using new, and should be garbage collected
     approximately once every second by calling gc.

     The argument to new is a function that is called if one or
     more fragments expire without being completely reassembled. *)

  type ip_packet = incoming * ip_header * incoming (* raw packet *)
  val new: ({header: ip_header, fragment: incoming,
	     raw_packet: incoming} -> unit) -> T
  val reassemble: T * ip_packet -> (ip_packet option * T)
  val gc: T -> T

 end (* sig *)

