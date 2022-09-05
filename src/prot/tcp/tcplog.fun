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

	tcplog.fun: functor for logging of the TCP implementation

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor TCP_LOG
	2.	type tcp_stats and function stats_string
	3.	type tcp_event
	4.	function event_makestring
	5.	function event_string
	6.	type time
	7.	internal function adjust_times
	8.	function relative_time

		iii.	RCS Log

$Log: tcplog.fun,v $
Revision 1.8  1996/05/16  23:50:57  esb
added the timeout to the round-trip-time-change event.

Revision 1.7  1996/05/14  01:22:22  esb
made time into an abstract type passed in as functor parameter.

Revision 1.6  1996/04/18  21:21:30  cline
updtated to match new TIME signature

Revision 1.5  1996/01/15  18:00:49  cline
removed FoxWord

Revision 1.4  1995/08/08  18:31:39  cline
upgraded to new signatures

Revision 1.3  1995/02/04  21:46:44  robby
updated to 107

Revision 1.2  1994/10/11  15:43:04  cline
Upgraded to work with SML/NJ 1.05b

Revision 1.1  1994/02/17  01:06:59  esb
Initial revision


*)
(*
	1.	functor TCP_LOG
*)

functor Tcp_Log (structure B: FOX_BASIS
		 type time
		 val time_makestring: time -> string
		 val zero_time: time
		 val delta: time * time -> time): TCP_LOG =
 struct

(*
	2.	type tcp_stats and function stats_string
*)

  datatype tcp_stats =
    Tcp_Stats of {packets_sent: Word64.word,
		  packets_received     : Word64.word,
		  packets_resent       : Word64.word,
		  bad_packets_received : Word64.word,
		  bytes_sent	       : unit -> Word64.word,
		  bytes_received       : unit -> Word64.word}
            
  fun stats_string (Tcp_Stats {packets_sent, packets_received,
			       packets_resent, bad_packets_received,
			       bytes_sent, bytes_received}) =
       "sent: 0x" ^ Word64.toString packets_sent ^
       ", received: 0x" ^ Word64.toString packets_received ^
       ", resent: 0x" ^ Word64.toString packets_resent ^
       ", bad: 0x" ^ Word64.toString bad_packets_received ^
       ", bytes sent: 0x" ^ Word64.toString (bytes_sent ()) ^
       ", received: 0x" ^ Word64.toString (bytes_received ())

(*
	3.	type tcp_event
*)

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

(*
	4.	function event_makestring
*)

  fun event_makestring (Retransmit {size}) =
       "retransmit(" ^ Integer.toString size ^ ")"
    | event_makestring (Congestion_Window_Change {new_size, old_size}) =
       "cw(" ^ Integer.toString old_size ^ "->" ^
       Integer.toString new_size ^ ")"
    | event_makestring (Round_Trip_Time_Change {new_rtt, old_rtt,
						new_rto, old_rto}) =
       "rtt(" ^ time_makestring old_rtt ^ "->" ^ time_makestring new_rtt ^
       ",timeout " ^ time_makestring old_rto ^ "->" ^ time_makestring new_rto ^
       ")"
    | event_makestring Bad_Packet_Received = "bad"
    | event_makestring (Packet_Sent {size, window, seq, ack}) =
       "sent" ^ Integer.toString size ^
       "(" ^ Word32.fmt StringCvt.DEC seq ^
       "/" ^ Word32.fmt StringCvt.DEC ack ^
       "+" ^ Integer.toString window ^ ")"
    | event_makestring (Packet_Received {size, window, seq, ack}) =
       "received" ^ Integer.toString size ^
       "(" ^ Word32.fmt StringCvt.DEC seq ^
       "/" ^ Word32.fmt StringCvt.DEC ack ^
       "+" ^ Integer.toString window ^ ")"
    | event_makestring (Empty_Sent {window, seq, ack}) =
       "sent0(" ^ Word32.fmt StringCvt.DEC seq ^
       "/" ^ Word32.fmt StringCvt.DEC ack ^
       "+" ^ Integer.toString window ^ ")"
    | event_makestring (Empty_Received {window, seq, ack}) =
       "received0(" ^ Word32.fmt StringCvt.DEC seq ^
       "/" ^ Word32.fmt StringCvt.DEC ack ^
       "+" ^ Integer.toString window ^ ")"

(*
	5.	function event_string
*)

  fun event_string (time, event) =
       event_makestring event ^ "@" ^ time_makestring time

(*
	6.	type time
*)

  type time = time

(*
	7.	internal function adjust_times
*)

  fun adjust_times (_, []) = []
    | adjust_times (NONE, (time, head) :: rest) =
       (zero_time, head) :: adjust_times (SOME time, rest)
    | adjust_times (SOME start, (time, head) :: rest) =
       (delta (time, start), head) :: adjust_times (SOME start, rest)

(*
	8.	function relative_time
*)

  fun relative_time list = adjust_times (NONE, list)

 end (* struct *)
