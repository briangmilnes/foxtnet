(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edoardo Biagioni (esb@cs.cmu.edu)
	Brian Milnes (Brian.Milnes@cs.cmu.edu)
	Nick Haines  (Nick.Haines@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

		i.	Abstract

	A one's complment 16 bit signed checksum module (as specified
	for TCP/IP/UDP). 

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	CHECKSUM

		iii.	RCS Log
	
$Log: checksum.sig,v $
Revision 1.8  1996/01/15  18:00:49  cline
removed FoxWord

Revision 1.7  1995/06/20  17:36:55  esb
adapted to word-arrays.

Revision 1.6  1994/09/30  16:26:30  esb
replaced Byte2 with FoxWord16

Revision 1.5  1994/02/08  14:09:45  esb
added checklistoff.

Revision 1.4  1993/12/23  23:13:23  esb
changed check_sum to checksum and added checklist.

Revision 1.3  1993/10/25  19:37:14  cline
removed .U from Byte[421].U

Revision 1.2  1993/07/23  16:14:53  esb
Added my name (which was unexplainably missing) and improved the comments.

Revision 1.1  1993/06/10  22:41:20  milnes
Initial revision


		1.	CHECKSUM
*)

signature CHECKSUM =
 sig

  (* Checksum_Bounds may be raised if the bounds are improper.
     Other exceptions could also be raised. *)
  exception Checksum_Bounds

  val one_s_add: Word16.word * Word16.word -> Word16.word

  val one_s_complement: Word16.word -> Word16.word

  (* checksum computes the checksum of the given array.
     The returned checksum is in machine byte order *)
  val checksum: Word_Array.T -> Word16.word

  (* A partial computation can be done in stages, one stage for
     each word_array that is part of the whole.  The computation
     MUST be done front-to-back and using the given partial states. *)
  type partial_state
  val initial_state: partial_state
  val check_partial: Word_Array.T * partial_state -> partial_state
  val complete_partial: partial_state -> Word16.word

 end


