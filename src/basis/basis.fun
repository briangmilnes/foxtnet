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
	1.	functor Fox_Basis

		iii.	RCS Log
	
$Log: basis.fun,v $
Revision 1.18  1995/04/05  15:12:04  esb
eliminated locatives, which were not being used.

Revision 1.17  1995/03/10  03:53:27  esb
adapted to new vendor.sig.

Revision 1.16  1995/03/07  20:30:58  esb
removed Debug_Trace, added a sharing constraint.

Revision 1.15  1995/02/04  20:39:06  robby
updated to 107

Revision 1.14  1995/01/14  02:26:16  esb
adapted to new filter.sig.

Revision 1.13  1994/11/10  16:14:37  milnes
Added the Debug_Trace substructure.

Revision 1.12  1994/10/19  23:03:47  esb
added Environment.

Revision 1.11  1994/09/30  16:47:08  esb
changed DXS to byte-extern, dlocs to dyn_locs

Revision 1.10  1994/09/19  18:23:15  robby
re-removed SEND_ and RECEIVE_PACKET

Revision 1.9  1994/08/26  21:47:38  robby
added receive and send packet.

Revision 1.8  1994/08/25  23:43:25  robby
added linearize

Revision 1.7  94/08/25  22:11:26  milnes
Added DLOCS and DXS.

Revision 1.6  1994/08/02  20:19:20  esb
replaced send_ and receive_message with dynamic arrays.

Revision 1.5  1994/07/04  21:41:11  esb
adapted to Copy/Create split.

Revision 1.4  1994/07/01  02:23:25  danwang
Moved control structures into Fox_Basis.

Revision 1.3  1994/06/16  21:49:20  danwang
Added sharing constraints

Revision 1.2  1994/06/16  16:11:18  danwang
New Fox_Basis functor

Revision 1.1  94/06/08  17:06:45  danwang
Initial revision

	1.	functor Fox_Basis
*) 

functor Fox_Basis (structure V: VENDOR
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
		   structure Filter: FILTER
		   structure Access: ACCESS
		   structure Dyn_Array: DYNAMIC_BYTE_ARRAY
		   structure Compare_Dyn_Array: COMPARE_ARRAY
		   structure Compare_Byte_Arrays: COMPARE_ARRAY
		   structure TimeUtil: TIMEUTIL
		   structure Time: TIME
		   structure Timing: TIMING
		   structure StopWatch: STOPWATCH
		   structure Scheduler: COROUTINE
		   structure Event_Queue: EVENT_QUEUE
		   structure Pipe: DATA_PIPE
                   structure Byte_Extern: BYTE_EXTERN 
		   structure Linearize: BUILD_LINEARIZE
		    sharing type Time.time = Timing.Time.time
	                and type Timing.timing = StopWatch.Timing.timing
                        and type ByteArray.bytearray = Compare_Byte_Arrays.data
		        and type Dyn_Array.T =
			         Compare_Dyn_Array.data): FOX_BASIS =
 struct
  structure V = V
  structure Debug = Debug
  structure Test = Test
  structure Copy = Copy 
  structure Create = Create 
  structure Order = Order
  structure Store = Store
  structure Fifo = Fifo
  structure Deq = Deq 
  structure Checksum = Checksum
  structure Tabulate = Tabulate
  structure Environment = Environment
  structure Format = Format
  structure Access = Access
  structure Filter = Filter
  structure Dyn_Array = Dyn_Array
  structure Compare_Dyn_Array = Compare_Dyn_Array
  structure Compare_Byte_Arrays = Compare_Byte_Arrays
  structure TimeUtil = TimeUtil
  structure Time = Time
  structure Timing = Timing
  structure StopWatch = StopWatch
  structure Scheduler = Scheduler
  structure Event_Queue = Event_Queue
  structure Pipe = Pipe
  structure Byte_Extern = Byte_Extern 
  structure Linearize = Linearize
 end (* struct *)







