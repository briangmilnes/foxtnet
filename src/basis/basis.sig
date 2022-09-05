(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edoardo Biagioni (Edoardo.Biagioni@cs.cmu.edu)
	Brian Milnes (Brian.Milnes@cs.cmu.edu)
	Nick Haines  (Nick.Haines@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

		i.	Abstract

	Fox_Basis groups all of the "utility" structures that we pass
	into almost all of our functors. The discipline is to
	reference almost nothing from the SML basis except those
	defined by the definition. The exceptions to this are the
	bytes and a few other signatures.


		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	signature FOX_BASIS

		iii.	RCS Log
	
$Log: basis.sig,v $
Revision 1.34  1997/11/14  11:35:34  cline
changed rigid sharing constraing to where type

Revision 1.33  96/04/18  18:43:44  cline
Removed Swap, Time, and Linearize.

Revision 1.32  1996/02/23  19:51:43  cline
remover link_parser

Revision 1.31  1996/02/06  23:43:43  esb
added a structure Memo, with signature MEMOIZE.

Revision 1.30  1996/01/15  18:00:49  cline
removed FoxWord

Revision 1.29  1995/11/12  16:50:23  esb
added structure Event, shared Compare.T with Word_Array.W8.T
instead of Word_Array.T

Revision 1.28  1995/09/17  22:07:25  esb
added Time.

Revision 1.27  1995/08/16  21:27:00  esb
added Write_Once structure for write-once variables

Revision 1.26  1995/06/20  17:40:21  esb
eliminated several structures, added word arrays.

Revision 1.25  1995/04/05  15:12:04  esb
eliminated locatives, which were not being used.

Revision 1.24  1995/03/10  03:53:27  esb
adapted to new vendor.sig.

Revision 1.23  1995/03/07  20:30:58  esb
removed Debug_Trace, added a sharing constraint.

Revision 1.22  1995/02/04  20:39:06  robby
updated to 107

Revision 1.21  1994/11/10  16:14:37  milnes
Added the Debug_Trace substructure.

Revision 1.20  1994/10/19  23:03:47  esb
added Environment.

Revision 1.19  1994/09/30  16:47:08  esb
changed DXS to byte-extern, dlocs to dyn_locs

Revision 1.18  1994/09/19  18:23:37  robby
re-removed SEND_ and RECEIVE_PACKET

Revision 1.17  1994/08/26  21:48:16  robby
added send and receive packet back in

Revision 1.16  1994/08/25  23:43:48  robby
added linearize

Revision 1.15  94/08/25  22:10:58  milnes
Added Dynamic Byte Array externs (DXS) and locatives (DLOCS).

Revision 1.14  1994/08/02  20:19:20  esb
replaced send_ and receive_message with dynamic arrays.

Revision 1.13  1994/07/04  21:41:11  esb
adapted to Copy/Create split.

Revision 1.12  1994/07/01  02:24:24  danwang
Moved control structures into Fox_Basis.

Revision 1.11  1994/06/16  18:45:38  danwang
Added sharing constraints.

Revision 1.10  1994/06/16  16:26:51  danwang
Updated to use functorized Fox_Basis.
Added TimingBoard and Send_Packet to the sig.

Revision 1.9  1994/02/21  00:10:22  esb
added Order.

Revision 1.8  93/10/29  04:55:37  esb
added structure Copy.

Revision 1.7  1993/10/06  17:21:22  esb
added the dispatcher structure.

Revision 1.6  1993/10/06  02:32:09  esb
added a store structure.

Revision 1.5  1993/09/17  16:51:47  milnes
Shortened that name of format and access.

Revision 1.4  1993/09/16  16:37:24  milnes
Commented and shortened Format and Access's names.

Revision 1.3  1993/09/13  22:08:04  cline
deleted '#'s from RCS log

Revision 1.2  1993/09/02  15:10:13  esb
removed the scheduler from the basis.

Revision 1.1  1993/08/27  20:32:46  esb
Initial revision


	1.	signature FOX_BASIS
*)

signature FOX_BASIS =
 sig
  structure V: VENDOR
  structure Debug: DEBUG
  structure Test: TEST
  structure Copy: COPY
  structure Create: CREATE
  structure Order: BYTE_ORDERS
  structure Store: STORE
  structure Fifo: FIFO
  structure Deq: DEQ
  structure Checksum: CHECKSUM
  structure Tabulate: TABULATE
  structure Environment: UNIX_ENVIRONMENT
  structure Format: FORMAT
  structure Access: ACCESS
  structure Filter: FILTER
  structure Compare: COMPARE_SEQ where type T = Word_Array.W8.T
  structure Scheduler: COROUTINE
  structure Event: SIMPLE_EVENT_QUEUE
  structure Event_Queue: EVENT_QUEUE
  structure Pipe: DATA_PIPE
  structure Semaphore: SEMAPHORE
  structure Write_Once: WRITE_ONCE
  structure Memo: MEMOIZE
 end (* sig *)
