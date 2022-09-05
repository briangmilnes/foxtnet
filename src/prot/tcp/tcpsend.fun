(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edo Biagioni (esb@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

	i.	Abstract

	tcpstate.fun: the implementation for the TCP finite state machine.
	Page numbers in the comments refer to page numbers in RFC 793,
	the definition of TCP [793]. Other commonly-referred to documents are:
	[1122] RFC 1122, Requirements for Internet Hosts (1989)
	[VJ88] Van Jacobson, Congestion Avoidance and Control,
	       ACM SIGCOMM-88, August 1988, pp 314-329
	[KP87] Karn and Partridge, Improving Round-Trip Time Estimates
	       in Reliable Transport Protocols, ACM SIGCOMM-87,
	       August 1987, pp 2-7


	ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor Tcp_Send
	2.	internal error function no_connection
	3.	internal error function connection_closing
	4.	internal function make_segment
	5.	internal function split_segment
	6.	internal function next_buffer
	7.	internal function restart_timer
	8.	internal function can_send
	9.	internal function get_data_size
	10.	internal function retransmit_queued
	11.	internal function send_tcb
	12.	function send_queued
	13.	internal function send_packet
	14.	function send_state


	iii.	RCS Log

$Log: tcpsend.fun,v $
Revision 1.38  1997/06/04  11:47:49  esb
changed to go along with new tcptcb.sig.

Revision 1.37  97/04/22  11:25:39  esb
adapted to new tcptcb.sig by removing the optional packet send function.

Revision 1.36  97/01/21  22:56:37  esb
took out double-segmentation code.

Revision 1.35  1996/10/18  20:49:07  esb
changed send to send_queued, added retransmission when window is zero.

Revision 1.34  1996/06/11  03:30:36  esb
adapted to new timer interface.

Revision 1.33  1996/05/16  23:53:11  esb
a large number of bug fixes.

Revision 1.32  1996/05/14  01:26:02  esb
changed to support timestamp options.

Revision 1.31  1996/04/18  21:23:54  cline
replaced makestring with Int.toString

Revision 1.30  1996/03/12  22:25:49  esb
adapted to new FOXWORD.

Revision 1.29  1996/01/19  23:03:30  esb
adapted to the new wordarray signature.

Revision 1.28  1996/01/15  18:00:49  cline
removed FoxWord

Revision 1.27  1995/09/14  21:11:39  cline
work around for representation bug

Revision 1.26  1995/08/08  18:31:39  cline
upgraded to new signatures

Revision 1.25  1995/03/12  17:52:02  esb
adapted to new trace.sig.

Revision 1.24  1995/03/07  23:57:08  esb
updated tracing.

Revision 1.23  1995/02/09  19:51:38  esb
corrected a comment.

Revision 1.22  1995/02/04  21:46:50  robby
updated to 107

Revision 1.21  1994/10/11  15:43:04  cline
Upgraded to work with SML/NJ 1.05b

Revision 1.20  1994/08/23  15:47:49  esb
fixed a state machine bug which occasionally caused infinite loops

Revision 1.19  1994/08/18  20:29:46  esb
added urgent support.

Revision 1.18  1994/08/17  16:28:26  esb
minor tuning and bug fixes.

Revision 1.17  1994/08/12  06:27:15  esb
converted to the new protocol signature.

Revision 1.16  1994/07/11  17:52:56  esb
optimized to use debug_print.

Revision 1.15  1994/07/07  02:30:01  esb
fixed a bug which was due to setting cwnd to zero if it exceeded 65K.

Revision 1.14  1994/06/16  16:42:04  danwang
Updated to use functorized Fox_Basis

Revision 1.13  1994/06/13  23:13:13  esb
moved timer implementation from tcpaction to tcptcb.

Revision 1.12  1994/05/10  08:03:45  esb
added optimizations.

Revision 1.11  94/03/25  16:21:49  esb
no longer adds a close_after_sends action in probe in some states.

Revision 1.10  94/03/11  04:58:55  esb
send now reports whether it succeeded or failed.

Revision 1.9  1994/03/07  16:52:13  esb
added a push at the end of each burst.

Revision 1.8  94/02/21  00:07:23  esb
fixed a bug whereby the congestion window was being ignored.

Revision 1.7  94/02/17  01:09:04  esb
changed to unacked_segs.

Revision 1.6  94/02/14  14:28:16  esb
added more accurate tracing messages.

Revision 1.5  1994/01/30  20:59:55  esb
added TCP slow start.

Revision 1.4  1994/01/28  01:38:45  esb
many changes and bug fixes.

Revision 1.3  1994/01/19  21:30:53  esb
adapted to new interface.

Revision 1.2  1994/01/09  03:21:21  esb
first functional version.

Revision 1.1  1993/11/11  05:31:56  esb
Initial revision


	1.	functor Tcp_Send
*)

functor Tcp_Send (structure Tcp_Tcb: TCP_TCB
		  structure Outgoing: EXTERNAL
		  structure Retransmit: TCP_RETRANSMIT
		  structure B: FOX_BASIS
		  sharing type Tcp_Tcb.tcp_tcb = Retransmit.Tcb.tcp_tcb
		      and type Tcp_Tcb.tcp_out = Retransmit.Tcb.tcp_out
		      and type Tcp_Tcb.outgoing_data = Outgoing.T
		  val debug_level: int ref option): TCP_SEND =
 struct
  type tcp_state = Tcp_Tcb.tcp_state
  type tcp_out = Tcp_Tcb.tcp_out
  type send_packet = Outgoing.T
  type segment = Tcp_Tcb.tcp_out

  datatype send_state = Valid_Send | Opening_Send | Closing_Send

  val zero32 = Word32.fromInt 0
  val one32 = Word32.fromInt 1
  val two32 = Word32.fromInt 2
  val hundred32 = Word32.fromInt 100

  local

   structure Trace = Trace (structure V = B.V
			    val debug_level = debug_level
			    val module_name = "tcpsend.fun"
			    val makestring = fn _ => NONE)

(*
	2.	internal error function no_connection

	[793] p. 41
*)

   fun no_connection (name, tcb as (Tcp_Tcb.Tcb {to_do, ...})) =
        let val action = Tcp_Tcb.User_Error (name ^ ": connection is closed")
        in to_do := Tcp_Tcb.Q.add (! to_do, action);
	   tcb
        end

(*
	3.	internal error function connection_closing

	[793] p. 41

	The send function in tcpmain.fun should detect when we are
	sending in an illegal state and raise a corresponding exception,
	so this function should never be called.  If it is, something
	is seriously wrong, so we close the connection, stop the window
	timer, and delete any window probes from the to_do list.  This
	will keep this error from recurring.
*)

   fun to_do_string (action, "") = Tcp_Tcb.action_string action
     | to_do_string (action, rest) =
        Tcp_Tcb.action_string action ^ ", " ^ rest

   fun time_wait_connection_closing (name, to_do) =
        to_do := Tcp_Tcb.Q.add (! to_do, Tcp_Tcb.User_Error
				(name ^ ": connection closing"))

   fun connection_closing (name, tcb as (Tcp_Tcb.Tcb {to_do, timers, ...})) =
        let val error = Tcp_Tcb.User_Error (name ^ ": connection closing")
	    val close = Tcp_Tcb.Delete_Tcb
	    fun nullify_action Tcp_Tcb.Probe_Window =
	         Tcp_Tcb.User_Error "cleared window probe"
	      | nullify_action action = action
        in to_do := Tcp_Tcb.Q.add (! to_do, error);
           to_do := Tcp_Tcb.Q.add (! to_do, close);
	   to_do := Tcp_Tcb.Q.map nullify_action (! to_do);
	   (#stop_window timers) ();
	   Trace.local_print ("connection closing, to_do list is " ^
			      Tcp_Tcb.Q.fold to_do_string (! to_do) "" ^
			      ", tcb is " ^
			      Tcp_Tcb.tcb_string tcb);
	   tcb
	end

(*
	4.	internal function make_segment
*)

   fun make_segment (tcb, data, length, false) =
        let val Tcp_Tcb.Tcb {snd_nxt, rcv_wnd, rcv_nxt,
			     rcv_wnd_scale, ts, ...} = tcb
	    val wnd = Word32.>> (! rcv_wnd, ! rcv_wnd_scale)
	in case ! ts of
	      NONE =>
	       Tcp_Tcb.Fast_Out {seq = ! snd_nxt, ack = ! rcv_nxt,
				 len = length, wnd = wnd, data = data}
	    | SOME {recent, last_ack_sent} =>
	       (last_ack_sent := ! rcv_nxt;
		Tcp_Tcb.Timestamp_Out {seq = ! snd_nxt, ack = ! rcv_nxt,
				       len = length, wnd = wnd, data = data,
				       times =
				       {send_time = Tcp_Tcb.current_time (),
					echo = ! recent}})
        end
     | make_segment (tcb, data, length, true) =	(* urgent *)
        let val Tcp_Tcb.Tcb {snd_nxt, rcv_wnd, rcv_nxt,
			     rcv_wnd_scale, ts, ...} = tcb
	    val wnd = Word32.>> (! rcv_wnd, ! rcv_wnd_scale)
	    val up = Word32.- (length, one32)
	    val options = case ! ts of
	                     NONE => []
	                   | SOME {recent, last_ack_sent} =>
			      (last_ack_sent := ! rcv_nxt;
			       [Tcp_Tcb.Timestamp
			         {send_time = Tcp_Tcb.current_time (),
				  echo = ! recent}])
	    val segment = Tcp_Tcb.Seg {seq = ! snd_nxt, ack = ! rcv_nxt,
				       len = length, wnd = wnd,
				       up = up, options = options,
				       syn_flag = false, fin_flag = false,
				       reset_flag = false, ack_flag = true,
				       push_flag = true, urgent_flag = true}
	in Tcp_Tcb.Out_Seg {seg = segment, data = data}
        end

(*
	5.	internal function split_segment

	Split the segment into chunks of the specified size
	and place all but the first chunk back onto the queue.
	Order is important.
*)

   fun split_segment (data, split, tcb, queue, urg) =
        let val word_split = Word.fromInt (Word32.toInt split)
 	    val rest = (Word32.fromInt o Word.toInt)
		       (Outgoing.size data - word_split)
 	    val (packet1, packet2) = Outgoing.split (data, word_split)
	in (make_segment (tcb, packet1, split, urg), split,
	    Tcp_Tcb.D.add_to_front (queue, (packet2, rest, urg)))
        end

(*
	6.	internal function next_buffer

	This function returns the first buffer, if
	- there is only one buffer (or no buffer), or
	- the sum of the lengths of the first two buffers exceeds the limit
	Otherwise, this function combines the first two buffers
	and tries again.

	This combining function is used to send fewer packets across
	the net and fewer TCP headers.  It can be very useful when
	sending large numbers of small packets, but it can also be
	slow since it copies the data twice.
*)

   local
    fun get_next (first as (first_packet, first_length, true), queue, _) =
         (queue, first)			(* urgent, send by itself. *)
      | get_next (first as (first_packet, first_length, _), queue, limit) =
         if Word32.>= (first_length, limit) then (queue, first)
	 else
	  case Tcp_Tcb.D.first queue of
	     NONE => (queue, first)
	   | SOME (new_queue, (_, _, true)) =>
	      (queue, first)		(* urgent data, don't merge*)
	   | SOME (new_queue,
		   second as (second_packet, second_length, _)) =>
	      if Word32.>= (Word32.+ (first_length, second_length),
			       limit) then
	       (queue, first)
	      else		(* combine the first two buffers *)
	       let val packet = Outgoing.join (first_packet, second_packet)
 		   val length = Word32.fromInt (Word.toInt
						(Outgoing.size packet))
	       in get_next ((packet, length, false),
			    new_queue, Word32.- (limit, length))
	       end

   in (* local *)
    fun next_buffer (queue, limit) =
         case Tcp_Tcb.D.first queue of
	    NONE => NONE
	  | SOME (new_queue, first) =>
	     SOME (get_next (first, new_queue, limit))
   end (* local *)

(*
	7.	internal function restart_timer
*)

   fun restart_timer (tcb as (Tcp_Tcb.Tcb {to_do, srto, timers, ...})) =
        ((#start_window timers) (! srto);
	 tcb)

(*
	8.	internal function can_send

	This function tries to implement part of the sender
	Silly-Window-Syndrome avoidance algorithm (RFC 1122, p. 98,
	4.2.3.4). A segment can be sent only if one of the following holds:
	- min (window, data queued) >= mss
	- unacked = 0 and all queued data fits in the window
	- unacked = 0 and 1/2 max_snd_wnd <= min (data queued, window)
	- the window timer has expired
	The last case is recognized by max_snd_wnd being zero.

	Note if unacked = 0, then can_send fails exactly when
	data size > window, window < mss, max_wnd <> 0, and
        max_wnd > window * 2.
 *)

  local
   open Word32	(* all arithmetic here is 32-bit unsigned *)

   val toString = Word32.fmt StringCvt.DEC
  in

   fun can_send (window, unacked:Word32.word, max_wnd, mss, data_size) =
        if data_size <= window then
	 data_size >= mss orelse unacked = zero32 orelse max_wnd = zero32
	else
	 window >= mss orelse
	 (unacked = zero32 andalso max_wnd <= window * two32) orelse
	 max_wnd = zero32

   fun can_string (window, unacked, max_wnd, mss, data_size) =
        "can_send (window " ^ toString window ^
        ", unacked " ^ toString unacked ^
        ", max_wnd " ^ toString max_wnd ^
        ", mss " ^ toString mss ^
        ", data_size " ^ toString data_size ^ ") = " ^
	Bool.toString (can_send (window, unacked, max_wnd, mss, data_size))

  end (* local *)

(*
	9.	internal function get_data_size
*)

   fun get_data_size (NONE, _) = zero32
     | get_data_size (SOME (data_queue, (packet, size, _)), max) =
        let val new_size = Word32.fromInt (Word.toInt (Outgoing.size packet))
	in if Word32.>= (new_size, max) then new_size
	   else Word32.+ (new_size,
			  get_data_size (Tcp_Tcb.D.first data_queue,
					 Word32.- (max, new_size)))
	end

(*
	10.	internal function retransmit_queued

	If the first segment in the retransmit queue follows one that
	has been sent before but has not itself been retransmitted,
	retransmit it now, since this function is called when we get a
	new ack.  If we add the timer expiration to the to_do list, we
	also expire the timer so we don't have duplicate expirations.
*)

   fun retransmit_queued (tcb as Tcp_Tcb.Tcb {to_do, resend, timers, ...}) =
        case Tcp_Tcb.D.first (! resend) of
	   NONE => tcb 
	 | SOME (new_queue, (_, _, Tcp_Tcb.Queued_After_Resent)) =>
	    let val expire = Tcp_Tcb.Timer_Expiration Tcp_Tcb.Resend_Timer
	        val {stop_resend, ...} = timers
	    in stop_resend ();
	       to_do := Tcp_Tcb.Q.add (! to_do, expire);
	       tcb
	    end
	 | _ => tcb

(*
	11.	internal function send_tcb

	Call retransmit_queued on all paths that end in not sending
	due to insufficient window, since the window may have been
	reduced following a packet drop.
*)

   fun send_tcb (tcb, in_loop) =
        let val Tcp_Tcb.Tcb {snd_nxt, snd_wnd, snd_una, max_snd_wnd,
			     send_immediately, unacked_segs,
			     mss, srtt, srto, cwnd, ssthresh,
			     queued, to_do, timers, rcv_sws, ...} = tcb
	    val debug_snd_nxt = ! snd_nxt
	    val unacked = Word32.- (! snd_nxt, ! snd_una)
	    val send_window = ! snd_wnd
	    val congestion_window = Word32.fromInt (! cwnd)
	    val total_window = Word32.min (send_window, congestion_window)
	    val window_size = Word32.- (total_window,
					Word32.min (total_window, unacked))
	    val b4mss = Word32.fromInt (Word16.toInt (! mss))
             (* when checking the data size, the largest useful number
                is the larger of the MSS and the window size. *)
	    val data_queue = ! queued
	    val data_size = get_data_size (Tcp_Tcb.D.first data_queue,
					   Word32.max (b4mss, window_size))
	    val send_limit = Word32.min (window_size, b4mss)
	     (* When next_buffer sees two packets whose total
	        length is less than the limit, it combines them.  We
		only want to combine very small packets, so we use
		a limit of at most 100. *)
	    val combine_limit = Word32.min (send_limit, hundred32)
	     (* for can_send, use an "effective" unacked size of zero,
	        since sending the previous segments always sets unacked
		to non-zero and if the data fits in the window,
		we want to send this data as part of the loop. *)
	    val effective_unacked = if in_loop then zero32 else unacked
             (* if there is a zero send window and there are still
                unacked segments, we do nothing. Otherwise, we try to send. *)
	in if window_size <> zero32 orelse unacked = zero32 then
	     (* send immediately if the flag is on or we can send an
                MSS-sized packet or it's time to probe the window or
                other extenuating circumstances (the max send window
                is small, or all queued data fits in the window.)
                These conditions are captured by can_send.  *)
	    if (! send_immediately) orelse	(* Nagle algorithm *)
	       can_send (window_size, effective_unacked, ! max_snd_wnd, b4mss,
			 data_size) then
	     case next_buffer (data_queue, combine_limit) of
	        NONE =>	(* no data to send, no timers to start. *)
		 tcb
	      | SOME (dequeued, (first_buffer, buffer_size, urg)) =>
		 let val (segment, segment_size, final_queued) =
		      if Word32.<= (buffer_size, send_limit) then
		       (* send the whole buffer.  Since send_packet
			  enqueues packets in mss-sized buffers,
			  this should be the common case. *)
		       (make_segment (tcb, first_buffer, buffer_size, urg),
			buffer_size, dequeued)
		      else
		      (* probe the window: send as much as fits in
		         the window but no less than one byte. *)
		       let val split = Word32.max (one32, send_limit)
		       in split_segment (first_buffer, split, tcb,
					 dequeued, urg)
		       end
		     val send_action = Tcp_Tcb.Send_Segment segment
		     val send_list = Tcp_Tcb.Q.add (! to_do, send_action)
		     val new_snd_nxt = Word32.+ (! snd_nxt, segment_size)
		 in snd_nxt := new_snd_nxt;
		    queued := final_queued;
		    to_do := send_list;
		    unacked_segs := 0;
		    Retransmit.retransmit (tcb, segment, ! srto);
		    if Word32.<= (b4mss, window_size) orelse
		       Word32.<= (segment_size, window_size) then
		     send_tcb (tcb, true)
		    else		(* window < mss & window < segment *)
		     (Trace.debug_constant_string "window full, send complete";
		      retransmit_queued tcb)
	         end (* let *)
	    else (* not send_immediately and can_send = false. *)
	     (* if the inability to send is NOT due to our having
	        outstanding unacked segments, then we better start
		the timer so we will try again to send later -- it
		may possibly be due to the receiver reducing its
		buffer space/window (RFC 1122, p. 99).  *)
	     (Trace.debug_print (fn _ =>
				 can_string (window_size, effective_unacked,
					     ! max_snd_wnd, b4mss, data_size));
	      if unacked = zero32 then
	       (Trace.debug_constant_string "restarting timer";
	        restart_timer tcb)
	      else
	       retransmit_queued tcb)
	   else
	    (* window_size = 0 andalso unacked <> 0; set timer and wait. *)
	    (Trace.debug_print (fn _ =>
				"send window exhausted, window " ^
				Word32.fmt StringCvt.DEC window_size ^
				", first unacked " ^
				Word32.fmt StringCvt.DEC (! snd_una) ^
				", next " ^
				Word32.fmt StringCvt.DEC (! snd_nxt) ^
				", send window " ^
				Word32.fmt StringCvt.DEC (! snd_wnd) ^
				", sws " ^
				 Word32.fmt StringCvt.DEC (! rcv_sws) ^
				", cwnd " ^ Int.toString (! cwnd));
	     restart_timer tcb;
	     retransmit_queued tcb) (* zero window, wait for the next call. *)
	end (* let *)
        
(*
	12.	function send_queued
*)

  in (* local *)
   fun send_queued (Tcp_Tcb.Estab tcb) = Tcp_Tcb.Estab (send_tcb (tcb, false))
     | send_queued (Tcp_Tcb.Close_Wait tcb) =
        Tcp_Tcb.Close_Wait (send_tcb (tcb, false))
     | send_queued (Tcp_Tcb.Syn_Active tcb) =
        Tcp_Tcb.Syn_Active (send_tcb (tcb, false))
     | send_queued (Tcp_Tcb.Syn_Passive (tcb, max_size)) =
        Tcp_Tcb.Syn_Passive (send_tcb (tcb, false), max_size)
	(* if we're in Fin_Wait_1 and fin_sent (the boolean) is false,
	   we are waiting to send the fin until all pending packets
	   are sent, so we should definitely allow sends.
	   If the fin has been sent, on the other hand, we disallow sends. *)
     | send_queued (Tcp_Tcb.Fin_Wait_1 (tcb, false)) =
        Tcp_Tcb.Fin_Wait_1 (send_tcb (tcb, false), false)
     | send_queued (Tcp_Tcb.Fin_Wait_1 (tcb, true)) =
        Tcp_Tcb.Fin_Wait_1 (connection_closing ("send(fw1/true)", tcb), true)
     | send_queued (Tcp_Tcb.Closed (to_do, timers)) =
        (Tcp_Tcb.Q.add (! to_do,
			Tcp_Tcb.User_Error "send connection closed");
	 Tcp_Tcb.Closed (to_do, timers))
     | send_queued (Tcp_Tcb.Listen (tcb, max_size)) =
        Tcp_Tcb.Listen (no_connection ("send(listen)", tcb), max_size)
     | send_queued (Tcp_Tcb.Syn_Sent (tcb, max_size)) =
        Tcp_Tcb.Syn_Sent (no_connection ("send(syn-sent)", tcb), max_size)
     | send_queued (Tcp_Tcb.Fin_Wait_2 tcb) =
        Tcp_Tcb.Fin_Wait_2 (connection_closing ("send(fw2)", tcb))
     | send_queued (Tcp_Tcb.Closing tcb) =
        Tcp_Tcb.Closing (connection_closing ("send(fw2+)", tcb))
     | send_queued (Tcp_Tcb.Last_Ack tcb) =
        Tcp_Tcb.Last_Ack (connection_closing ("send(la)", tcb))
     | send_queued (state as (Tcp_Tcb.Time_Wait {to_do, ...})) =
        (time_wait_connection_closing ("send(ftw)", to_do);
	 state)

(*
	13.	internal function send_packet

	[793] pp. 19-21

	Add the segment to the queue, then call send_queued.
*)

   exception Send_Packet of string

   fun send_packet (state, packet, urgent) =
        let val size = Outgoing.size packet
            val tcb = case state of
	                Tcp_Tcb.Estab tcb => tcb
		      | Tcp_Tcb.Close_Wait tcb => tcb
		      | Tcp_Tcb.Syn_Active tcb => tcb
		      | Tcp_Tcb.Syn_Passive (tcb, _) => tcb
		      | _ =>
			 let val state = Tcp_Tcb.state_string state
			     val string = "unexpected state " ^ state ^
			                  " in send_packet"
			 in Trace.print_raise (Send_Packet string, SOME string)
			 end
	    val Tcp_Tcb.Tcb {mss, queued, ...} = tcb
	    val max_segment = Word.fromInt (Word16.toInt (! mss))
	    val size32 = Word.toLargeWord size
          in if size <= 0w0 then state
	     else
	      (queued := Tcp_Tcb.D.add_to_back (! queued,
						(packet, size32, urgent));
	       send_queued state)
          end (* let *)

   end (* local *)
(*
	14.	function send_state
*)

  fun send_state (Tcp_Tcb.Estab _) = Valid_Send
    | send_state (Tcp_Tcb.Close_Wait _) = Valid_Send
    | send_state (Tcp_Tcb.Syn_Active _) = Valid_Send
    | send_state (Tcp_Tcb.Syn_Passive _) = Valid_Send
    | send_state (Tcp_Tcb.Listen _) = Opening_Send
    | send_state (Tcp_Tcb.Syn_Sent _) = Opening_Send
    | send_state (Tcp_Tcb.Closed _) = Closing_Send
    | send_state (Tcp_Tcb.Fin_Wait_1 _) = Closing_Send
    | send_state (Tcp_Tcb.Fin_Wait_2 _) = Closing_Send
    | send_state (Tcp_Tcb.Closing _) = Closing_Send
    | send_state (Tcp_Tcb.Last_Ack _) = Closing_Send
    | send_state (Tcp_Tcb.Time_Wait _) = Closing_Send

 end (* struct *)

