(*

        FoxNet: The Fox Project's Communication Protocol Implementation Effort
        Edo Biagioni (esb@cs.cmu.edu)
        Nick Haines (nickh@cs.cmu.edu)
        Brian Milnes (milnes@cs.cmu.edu)
        Fox Project
        School of Computer Science
        Carnegie Mellon University
        Pittsburgh, Pa 15139-3891

		i.	Abstract
	tcplog.sig: signature for logging of the TCP implementation


		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	signature TCP_LOG


		iii.	RCS Log

$Log: tcplog.sig,v $
Revision 1.8  1996/07/22  20:10:53  cline
*** empty log message ***

Revision 1.7  1996/05/16  23:50:57  esb
added the timeout to the round-trip-time-change event.

Revision 1.6  1996/05/14  01:21:59  esb
made time into an abstract type.

Revision 1.5  1996/01/15  18:00:49  cline
removed FoxWord

Revision 1.4  1995/08/08  18:31:39  cline
upgraded to new signatures

Revision 1.3  1995/06/20  17:07:19  esb
adapted to new protocol signature.

Revision 1.2  1994/10/11  15:43:04  cline
Upgraded to work with SML/NJ 1.05b

Revision 1.1  1994/02/17  01:06:58  esb
Initial revision


*)
(*
	1.	signature TCP_LOG

	for debugging purposes it is useful to know the significant
	events in a TCP's history without significantly slowing down
	the TCP itself. Stats keeping and event logging can be turned
	on with the corresponding control operations, and the results
	read with the corresponding info commands. Logging is off when
	a connection is first opened and by default.
*)

signature TCP_LOG =
 sig

  datatype tcp_stats = Tcp_Stats of {bytes_sent: unit -> Word64.word,
				     bytes_received: unit -> Word64.word,
				     packets_sent: Word64.word,
				     packets_received: Word64.word,
				     packets_resent: Word64.word,
				     bad_packets_received: Word64.word}
            
  val stats_string: tcp_stats -> string

  type time

  datatype tcp_event =
       Retransmit of {size: int}
     | Congestion_Window_Change of {new_size: int, old_size: int}
     | Round_Trip_Time_Change of {new_rtt: time, old_rtt: time,
				  new_rto: time, old_rto: time}
     | Bad_Packet_Received
     | Packet_Sent of {size: int, window: int,
		       seq: Word32.word, ack: Word32.word}
     | Packet_Received of {size: int, window: int,
			   seq: Word32.word, ack: Word32.word}
     | Empty_Sent of {window: int, seq: Word32.word, ack: Word32.word}
     | Empty_Received of {window: int, seq: Word32.word,
			  ack: Word32.word}

  val event_makestring: tcp_event -> string

  val event_string: time * tcp_event -> string

  (* relative_time makes time relative to the first time in the list. *)
  val relative_time: (time * tcp_event) list ->
                     (time * tcp_event) list

 end (* sig *)
