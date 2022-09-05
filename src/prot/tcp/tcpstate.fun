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
	1.	functor Tcp_State
	2.	internal function generate_iss
	3.	internal value empty_data
	4.	internal functions create_empty and send_empty
	5.	internal error functions
	6.	function active_open
	7.	function passive_open
	8.	function close
	9.	internal function abort_existing
	10.	function abort
	11.	internal function get_tcb
	12.	internal function send_ack
	13.	function timeout


	iii.	RCS Log

$Log: tcpstate.fun,v $
Revision 1.40  1997/06/04  11:48:21  esb
changed to go along with new tcptcb.sig.

Revision 1.39  97/04/22  11:26:15  esb
adapted to new tcptcb.sig.

Revision 1.38  97/01/24  15:00:06  cline
replaced quot with Int.quot

Revision 1.37  1996/12/18  17:07:52  esb
changed it to only send ack if no segments are in the queue waiting
to be processed (in the to_do list).

Revision 1.36  1996/10/18  20:48:26  esb
now only increases srto when resending the same segment.

Revision 1.35  1996/08/16  18:15:04  esb
fixed RCS or the file system truncating the very end of the file.

Revision 1.34  1996/08/16  17:36:56  esb
fixed a bug that would silently raise "Implementation_Error"

Revision 1.33  1996/06/11  03:27:24  esb
reorganized the timeout code.

Revision 1.32  1996/05/14  01:26:13  esb
changed to support timestamp options.

Revision 1.31  1996/04/18  21:24:14  cline
updated to match new TIME signature

Revision 1.30  1996/01/19  23:03:30  esb
adapted to the new wordarray signature.

Revision 1.29  1996/01/15  18:00:49  cline
removed FoxWord

Revision 1.28  1995/08/08  18:31:39  cline
upgraded to new signatures

Revision 1.27  1995/03/12  17:52:02  esb
adapted to new trace.sig.

Revision 1.26  1995/03/10  03:47:37  esb
adapted to new vendor.sig.

Revision 1.25  1995/03/07  23:57:08  esb
updated tracing.

Revision 1.24  1995/02/22  15:24:07  esb
minor changes.

Revision 1.23  1995/02/04  21:46:51  robby
updated to 107

Revision 1.22  1995/01/14  02:29:53  esb
added tcp window-scale option.

Revision 1.21  1994/10/11  15:43:04  cline
Upgraded to work with SML/NJ 1.05b

Revision 1.20  1994/08/23  15:48:18  esb
fixed a minor bug in logging.

Revision 1.19  1994/08/18  20:29:03  esb
adapted to changes in tcptcb.sig.

Revision 1.18  1994/08/17  16:28:26  esb
minor tuning and bug fixes.

Revision 1.17  1994/08/12  06:27:15  esb
converted to the new protocol signature.

Revision 1.16  1994/06/17  22:03:47  esb
added a complete_close to abort, to let the abort complete cleanly.

Revision 1.15  1994/06/16  16:42:04  danwang
Updated to use functorized Fox_Basis

Revision 1.14  1994/06/13  23:13:13  esb
moved timer implementation from tcpaction to tcptcb.

Revision 1.13  1994/05/10  08:05:47  esb
minor bug fixes in rtt computation, optimized.

Revision 1.12  94/04/15  03:20:18  esb
when going from last_ack to close, now puts delete_tcb on correct to_do list.

Revision 1.11  94/03/25  16:24:23  esb
made independent of probe_window by explicitly adding close_after_send.

Revision 1.10  94/02/25  18:35:12  milnes
Updated timing and moved to safe array operations.

Revision 1.9  1994/02/21  00:06:54  esb
minor change.

Revision 1.8  94/02/17  01:08:44  esb
improved ack generation.

Revision 1.7  94/02/14  14:29:19  esb
added logging of retransmissions.

Revision 1.6  1994/01/30  21:01:41  esb
added tcp slow start to the retransmission timer.

Revision 1.5  1994/01/28  01:30:09  esb
new interface; added window timeouts and their handling.

Revision 1.4  1994/01/19  21:31:58  esb
adapted to new interface, added exponential backoff.

Revision 1.3  1994/01/11  23:07:22  esb
added a complete_close when a non-existing connection is closed.

Revision 1.2  1994/01/09  03:20:46  esb
first functional version.

Revision 1.1  1993/11/11  05:31:56  esb
Initial revision


	1.	functor Tcp_State
*)

functor Tcp_State (structure Tcp_Tcb: TCP_TCB
		   structure Retransmit: TCP_RETRANSMIT
		   structure Incoming: EXTERNAL
		   structure Outgoing: EXTERNAL
		   sharing Tcp_Tcb = Retransmit.Tcb
		       and type Tcp_Tcb.incoming_data = Incoming.T
		       and type Tcp_Tcb.outgoing_data = Outgoing.T
		   val ack_time: Tcp_Tcb.time_ms
		   structure B: FOX_BASIS
		   val debug_level: int ref option): TCP_STATE =
 struct
  structure Trace = Trace (structure V = B.V
			   val debug_level = debug_level
			   val module_name = "tcpstate.fun"
			   val makestring = fn _ => NONE)

  type tcp_state = Tcp_Tcb.tcp_state
  type tcp_timer = Tcp_Tcb.tcp_timer
  type tcp_timer_set = Tcp_Tcb.tcp_timer_set

(*
	2.	internal function generate_iss

	[793] pp. 27-30
*)

  fun generate_iss () =
       let val now = B.V.Time.now ()
	   val usec = B.V.Time.- (now, B.V.Time.fromSeconds
				         (B.V.Time.toSeconds now))
           val sec32 = Word32.fromInt (B.V.Time.toSeconds now)
           val usec32 = Word32.fromInt (B.V.Time.toMicroseconds usec)
       (* iss must be computed using Word32 operations since we want
          wrap-around. *)
           val iss = Word32.+ (Word32.* (sec32, 0w1000000), usec32)
           fun print () = Trace.local_print ("new iss is " ^
					     Word32.fmt StringCvt.DEC iss)
       in Trace.do_if_debug print;
          iss
       end

(*
	3.	internal value empty_data
*)

  val empty_data = Outgoing.uninitialized 0w0

(*
	4.	internal functions create_empty and send_empty
*)

  val zero = (* 0w0 : Word32.word *) Word32.fromInt 0

  fun create_empty (seq, ack_value, wnd, options,
		    {syn, fin, rst, ack}) =
       let val seg = Tcp_Tcb.Seg {seq = seq, ack = ack_value, len = zero,
				  wnd = wnd, up = zero, options = options,
				  syn_flag = syn, fin_flag = fin,
				  reset_flag = rst, ack_flag = ack,
				  push_flag = false, urgent_flag = false}
	   val out = Tcp_Tcb.Out_Seg {seg = seg, data = empty_data}
	   val send = Tcp_Tcb.Send_Segment out
       in (send, out)
       end

  fun send_empty x =
       let val (send, _) = create_empty x
       in send
       end

(*
	5.	internal error functions
*)

  fun already_exists_actions to_do =
       let val action = Tcp_Tcb.User_Error "connection already exists"
       in to_do := Tcp_Tcb.Q.add (! to_do, action);
	  Trace.debug_constant_string "error, connection already exists"
       end

  fun already_exists (tcb as (Tcp_Tcb.Tcb {to_do, ...})) =
       (already_exists_actions to_do;
	tcb)

  fun close_error_actions to_do =
       let val action = Tcp_Tcb.User_Error "connection closing"
       in to_do := Tcp_Tcb.Q.add (! to_do, action);
	  Trace.debug_constant_string "error, connection closing"
       end

  fun close_error (tcb as (Tcp_Tcb.Tcb {to_do, ...})) =
       (close_error_actions to_do;
	tcb)

  fun user_timeout (tcb as (Tcp_Tcb.Tcb {to_do, timers, ...})) =
       let val message = "connection aborted due to user timeout"
       in Trace.debug_constant_string "connection aborted due to user timeout";
	  to_do := Tcp_Tcb.Q.add (! to_do, Tcp_Tcb.User_Error message);
	  to_do := Tcp_Tcb.Q.add (! to_do, Tcp_Tcb.Delete_Tcb);
          Tcp_Tcb.Closed (to_do, timers)
       end

(*
	6.	function active_open

	[793] pp. 54, 55
*)

  val zero32 = Word32.fromInt 0

  val max_unshifted_window = 0xffff	(* RFC 1323, p. 11 *)
  val max_window_shift = Word8.fromInt 14 (* RFC 1323, p. 11 *)
  val init_srtt = Word32.fromInt 0	(* RFC 1122, p. 96 *)
  val init_srtd = Word32.fromInt 6000	(* RFC 1122, p. 96 *)
  val init_srto = Word32.fromInt 3000	(* RFC 1122, p. 96 *)

  fun compute_window_shift window =
       let fun shift_loop (candidate, shift) =
	        if candidate <= max_unshifted_window then
		 (Word32.fromInt candidate, Word8.fromInt shift)
		else
		 shift_loop (Bits.>> (candidate, 0w1), shift + 1)
	in shift_loop (window, 0)
	end

  fun actively_open (to_do, iss, window, max_size, timers) =
       let val mss_size = Word16.fromInt max_size
	   val (wnd, wnd_scale) = compute_window_shift window
	   val seg_option = Tcp_Tcb.Max_Segment {size = mss_size}
	   val scale_option = Tcp_Tcb.Window_Scale {shift = wnd_scale}
	   val now = Tcp_Tcb.current_time ()
	   val ts_option = Tcp_Tcb.Timestamp {send_time = now, echo = zero32}
	   val options = [seg_option, scale_option, ts_option]
	   val (send_action, out_seg) =
	         create_empty (iss, zero, wnd, options,
			       {syn = true, fin = false, rst = false,
				ack = false})
	   val iss_inc = Word32.+ (iss, Word32.fromInt 1)
	   val mss_value = max_size	(* cwnd initialization *)
	   val _ = to_do := Tcp_Tcb.Q.add (! to_do, send_action)
	   val tcb = Tcp_Tcb.Tcb {iss = iss,
				  snd_una = ref iss,
				  snd_nxt = ref iss_inc,
				  snd_wnd = ref zero,
				  max_snd_wnd = ref zero,
				  snd_wl1 = ref iss,
				  snd_wl2 = ref zero,
				  snd_wnd_scale = ref 0w0,
				  irs = ref zero,
				  rcv_nxt = ref zero,
				  rcv_wnd = ref wnd,
				  rcv_sws = ref zero,
				  rcv_wnd_scale = ref 0w0,
				  wanted_wnd_scale = wnd_scale,
				  ts = ref NONE,
				  send_immediately = ref false,
				  cwnd = ref mss_value,
      (* start with a large ssthresh so we can grow quickly and
         quickly get an estimate for the real value of the congestion
         window (by dropping a packet, if the congestion window is
         smaller than the actual window). *)
				  ssthresh = ref window,
				  unacked_segs = ref 0,
				  srtt = ref init_srtt,
				  srtd = ref init_srtd,
				  srto = ref init_srto,
				  wto = ref zero32,
				  mss = ref (Word16.fromInt 536),
				  resend = ref (Tcp_Tcb.D.new ()),
				  queued = ref (Tcp_Tcb.D.new ()),
				  out_of_order = ref (Tcp_Tcb.Q.new ()),
				  timers = timers,
				  to_do = to_do}
       in Retransmit.retransmit (tcb, out_seg, init_srto);
	  Tcp_Tcb.Syn_Sent (tcb, max_size)
       end

  fun active_open (Tcp_Tcb.Closed (to_do, timers), window, max_size) =
       (Trace.debug_constant_string "active open of new connection";
	actively_open (to_do, generate_iss (), window, max_size, timers))
    | active_open (Tcp_Tcb.Listen (Tcp_Tcb.Tcb {to_do, timers, ...},
				   previous_max),
		   window, max_size) =
       (Trace.debug_constant_string "active open from Listen";
	active_open (Tcp_Tcb.Closed (to_do, timers), window, max_size))
    | active_open (Tcp_Tcb.Syn_Sent (tcb, max_size), _, _) =
       (Trace.debug_constant_string "Syn_Sent active open";
	Tcp_Tcb.Syn_Sent (already_exists tcb, max_size))
    | active_open (Tcp_Tcb.Syn_Active tcb, _, _) = 
       (Trace.debug_constant_string "Syn_Active active open";
	Tcp_Tcb.Syn_Active (already_exists tcb))
    | active_open (Tcp_Tcb.Syn_Passive (tcb, max_size), _, _) = 
       (Trace.debug_constant_string "Syn_Passive active open";
	Tcp_Tcb.Syn_Passive (already_exists tcb, max_size))
    | active_open (Tcp_Tcb.Estab tcb, _, _) = 
       (Trace.debug_constant_string "Estab active open";
	Tcp_Tcb.Estab (already_exists tcb))
    | active_open (Tcp_Tcb.Fin_Wait_1 (tcb, fin_sent), _, _) = 
       (Trace.debug_constant_string "Fin_Wait_1 active open";
	Tcp_Tcb.Fin_Wait_1 (already_exists tcb, fin_sent))
    | active_open (Tcp_Tcb.Fin_Wait_2 tcb, _, _) = 
       (Trace.debug_constant_string "Fin_Wait_2 active open";
	Tcp_Tcb.Fin_Wait_2 (already_exists tcb))
    | active_open (Tcp_Tcb.Close_Wait tcb, _, _) = 
       (Trace.debug_constant_string "Close_Wait active open";
	Tcp_Tcb.Close_Wait (already_exists tcb))
    | active_open (Tcp_Tcb.Closing tcb, _, _) = 
       (Trace.debug_constant_string "active open from Closing";
	Tcp_Tcb.Closing (already_exists tcb))
    | active_open (Tcp_Tcb.Last_Ack tcb, _, _) = 
       (Trace.debug_constant_string "Last_Ack active open";
	Tcp_Tcb.Last_Ack (already_exists tcb))
    | active_open (state as (Tcp_Tcb.Time_Wait {to_do, ...}), _, _) = 
       (Trace.debug_constant_string "Time_Wait active open";
	already_exists_actions to_do;
	state)

(*
	7.	function passive_open

	[793] pp. 54, 55
*)

  fun passively_open (to_do, iss, window, max_size, timers) =
       let val wnd = Word32.fromInt window
	   val mss_value = max_size	(* cwnd initialization *)
	   val (wnd, wnd_scale) = compute_window_shift window
	   val tcb = Tcp_Tcb.Tcb {iss = iss,
				  snd_una = ref iss,
				  snd_nxt = ref iss,
				  snd_wnd = ref zero,
				  max_snd_wnd = ref zero,
				  snd_wl1 = ref iss,
				  snd_wl2 = ref zero,
				  snd_wnd_scale = ref 0w0,
				  irs = ref zero,
				  rcv_nxt = ref zero,
				  rcv_wnd = ref wnd,
				  rcv_sws = ref zero,
				  rcv_wnd_scale = ref 0w0,
				  wanted_wnd_scale = wnd_scale,
				  ts = ref NONE,
				  send_immediately = ref false,
				  cwnd = ref mss_value,
      (* start with a large ssthresh so we can grow quickly and
         quickly get an estimate for the real value of the congestion
         window (by dropping a packet, if the congestion window is
         smaller than the actual window). *)
				  ssthresh = ref window,
				  unacked_segs = ref 0,
				  srtt = ref init_srtt,
				  srtd = ref init_srtd,
				  srto = ref init_srto,
				  wto = ref zero32,
				  mss = ref (Word16.fromInt 536),
				  resend = ref (Tcp_Tcb.D.new ()),
				  queued = ref (Tcp_Tcb.D.new ()),
				  out_of_order = ref (Tcp_Tcb.Q.new ()),
				  timers = timers,
				  to_do = to_do}
       in Tcp_Tcb.Listen (tcb, max_size)
       end

  fun passive_open (Tcp_Tcb.Closed (to_do, timers), window, max_size) =
       passively_open (to_do, generate_iss (), window, max_size, timers)
    | passive_open (state as (Tcp_Tcb.Listen (tcb, max)), _, _) =
       (already_exists tcb;
	state)
    | passive_open (state as (Tcp_Tcb.Syn_Sent (tcb, max)), _, _) =
       (already_exists tcb;
	state)
    | passive_open (state as (Tcp_Tcb.Syn_Active tcb), _, _) =
       (already_exists tcb;
	state)
    | passive_open (state as (Tcp_Tcb.Syn_Passive (tcb, max)), _, _) =
       (already_exists tcb;
	state)
    | passive_open (state as (Tcp_Tcb.Estab tcb), _, _) =
       (already_exists tcb;
	state)
    | passive_open (state as (Tcp_Tcb.Fin_Wait_1 (tcb, fin_sent)), _, _) =
       (already_exists tcb;
	state)
    | passive_open (state as (Tcp_Tcb.Fin_Wait_2 tcb), _, _) =
       (already_exists tcb;
	state)
    | passive_open (state as (Tcp_Tcb.Close_Wait tcb), _, _) =
       (already_exists tcb;
	state)
    | passive_open (state as (Tcp_Tcb.Closing tcb), _, _) =
       (already_exists tcb;
	state)
    | passive_open (state as (Tcp_Tcb.Last_Ack tcb), _, _) =
       (already_exists tcb;
	state)
    | passive_open (state as (Tcp_Tcb.Time_Wait {to_do, ...}), _, _) =
       (already_exists_actions to_do;
	state)

(*
	8.	function close

	[793] pp. 60, 61
*)

  fun has_send queued = not (Tcp_Tcb.D.empty queued)

  fun send_fin (tcb as (Tcp_Tcb.Tcb {to_do, snd_nxt, rcv_nxt, rcv_wnd,
				     srto, ...})) =
       let val (send_action, seg) =
	         create_empty (! snd_nxt, ! rcv_nxt, ! rcv_wnd, [],
			       {syn = false, fin = true, rst = false,
				ack = true})
       in to_do := Tcp_Tcb.Q.add (! to_do, send_action);
	  Retransmit.retransmit (tcb, seg, ! srto);
	  snd_nxt := Word32.+ (! snd_nxt, (* 0w1 *) Word32.fromInt 1)
       end (* let *)

  fun close (Tcp_Tcb.Closed (to_do, timers)) =
       (to_do :=
	   Tcp_Tcb.Q.add (! to_do,
			  Tcp_Tcb.User_Error "connection does not exist");
	to_do := Tcp_Tcb.Q.add (! to_do, Tcp_Tcb.Complete_Close false);
	Tcp_Tcb.Closed (to_do, timers))
    | close (Tcp_Tcb.Listen (Tcp_Tcb.Tcb {to_do, timers, ...}, max_size)) =
       (to_do := Tcp_Tcb.Q.add (! to_do, Tcp_Tcb.Delete_Tcb);
	Tcp_Tcb.Closed (to_do, timers))
    | close (Tcp_Tcb.Syn_Sent (Tcp_Tcb.Tcb {to_do, timers, ...}, _)) =
       (to_do := Tcp_Tcb.Q.add (! to_do, Tcp_Tcb.Delete_Tcb);
	Tcp_Tcb.Closed (to_do, timers))
    | close (Tcp_Tcb.Syn_Active (tcb as (Tcp_Tcb.Tcb {to_do, ...}))) =
       (to_do := Tcp_Tcb.Q.add (! to_do, Tcp_Tcb.Complete_Open true);
	close (Tcp_Tcb.Estab tcb))
    | close (Tcp_Tcb.Syn_Passive (tcb as (Tcp_Tcb.Tcb {to_do, ...}), _)) =
       (to_do := Tcp_Tcb.Q.add (! to_do, Tcp_Tcb.Complete_Open true);
	close (Tcp_Tcb.Estab tcb))
    | close (Tcp_Tcb.Estab (tcb as (Tcp_Tcb.Tcb {to_do, queued, ...}))) =
       if has_send (! queued) then
	(to_do := Tcp_Tcb.Q.add (! to_do, Tcp_Tcb.Probe_Window);
	 to_do := Tcp_Tcb.Q.add (! to_do, Tcp_Tcb.Close_After_Sends);
	 Tcp_Tcb.Fin_Wait_1 (tcb, false))
       else
	(send_fin tcb;
	 Tcp_Tcb.Fin_Wait_1 (tcb, true))
    | close (Tcp_Tcb.Fin_Wait_1 (tcb, true)) =
       (Trace.local_print "Fin_Wait_1 close";
	Tcp_Tcb.Fin_Wait_1 (tcb, true))
    | close (Tcp_Tcb.Fin_Wait_1 (tcb as Tcp_Tcb.Tcb {queued, to_do, ...},
				 false)) =
       (if has_send (! queued) then
	 (to_do := Tcp_Tcb.Q.add (! to_do, Tcp_Tcb.Probe_Window);
	  to_do := Tcp_Tcb.Q.add (! to_do, Tcp_Tcb.Close_After_Sends);
	  Tcp_Tcb.Fin_Wait_1 (tcb, false))
	else
	 (send_fin tcb;
	  Tcp_Tcb.Fin_Wait_1 (tcb, true)))
    | close (Tcp_Tcb.Fin_Wait_2 tcb) =
       Tcp_Tcb.Fin_Wait_2 tcb
    | close (Tcp_Tcb.Close_Wait (tcb as (Tcp_Tcb.Tcb {to_do, queued, ...}))) =
       if has_send (! queued) then
	(to_do := Tcp_Tcb.Q.add (! to_do, Tcp_Tcb.Probe_Window);
	 to_do := Tcp_Tcb.Q.add (! to_do, Tcp_Tcb.Close_After_Sends);
	 Tcp_Tcb.Close_Wait tcb)
       else
	(send_fin tcb;
	 Tcp_Tcb.Last_Ack tcb)
    | close (Tcp_Tcb.Closing tcb) =
       (Trace.local_print "Closing close";
	Tcp_Tcb.Closing (close_error tcb))
    | close (Tcp_Tcb.Last_Ack tcb) =
       (Trace.local_print "Last_Ack close";
	Tcp_Tcb.Last_Ack (close_error tcb))
    | close (state as (Tcp_Tcb.Time_Wait {to_do, ...})) =
       (Trace.local_print "Time_Wait close";
	close_error_actions to_do;
	state)

(*
	9.	internal function abort_existing
*)

  fun abort_existing tcb =
       let val Tcp_Tcb.Tcb {snd_nxt, rcv_wnd, to_do, timers, ...} = tcb
	   val send = send_empty (! snd_nxt, zero, ! rcv_wnd, [],
				  {syn = false, fin = false, rst = true,
				   ack = false})
	   val close = Tcp_Tcb.Complete_Close true
	   val delete = Tcp_Tcb.Delete_Tcb
       in to_do := Tcp_Tcb.Q.add (! to_do, send);
	  to_do := Tcp_Tcb.Q.add (! to_do, close);
	  to_do := Tcp_Tcb.Q.add (! to_do, delete);
          Tcp_Tcb.Closed (to_do, timers)
       end

(*
	10.	function abort
*)

  fun abort (Tcp_Tcb.Closed (to_do, timers)) =
       (to_do :=
	   Tcp_Tcb.Q.add (! to_do,
			  Tcp_Tcb.User_Error "connection does not exist");
	Tcp_Tcb.Closed (to_do, timers))
    | abort (Tcp_Tcb.Listen (Tcp_Tcb.Tcb {to_do, timers, ...}, max_size)) =
       (to_do := Tcp_Tcb.Q.add (! to_do, Tcp_Tcb.Delete_Tcb);
	Tcp_Tcb.Closed (to_do, timers))
    | abort (Tcp_Tcb.Syn_Sent (Tcp_Tcb.Tcb {to_do, timers, ...}, _)) =
       (to_do := Tcp_Tcb.Q.add (Tcp_Tcb.Q.new (), Tcp_Tcb.Delete_Tcb);
	Tcp_Tcb.Closed (to_do, timers))
    | abort (Tcp_Tcb.Syn_Active tcb) = abort_existing tcb
    | abort (Tcp_Tcb.Syn_Passive (tcb, _)) = abort_existing tcb
    | abort (Tcp_Tcb.Estab tcb) = abort_existing tcb
    | abort (Tcp_Tcb.Fin_Wait_1 (tcb, _)) = abort_existing tcb
    | abort (Tcp_Tcb.Fin_Wait_2 tcb) = abort_existing tcb
    | abort (Tcp_Tcb.Close_Wait tcb) = abort_existing tcb
    | abort (Tcp_Tcb.Closing (Tcp_Tcb.Tcb {to_do, timers, ...})) =
       (to_do := Tcp_Tcb.Q.add (Tcp_Tcb.Q.new (), Tcp_Tcb.Delete_Tcb);
	Tcp_Tcb.Closed (to_do, timers))
    | abort (Tcp_Tcb.Last_Ack (Tcp_Tcb.Tcb {to_do, timers, ...})) =
       (to_do := Tcp_Tcb.Q.add (Tcp_Tcb.Q.new (), Tcp_Tcb.Delete_Tcb);
	Tcp_Tcb.Closed (to_do, timers))
    | abort (state as (Tcp_Tcb.Time_Wait _)) = state

(*
	11.	internal function get_tcb
*)

  exception Tcp_State_Implementation_Error

  fun get_tcb (Tcp_Tcb.Closed _) =
       Trace.print_raise (Tcp_State_Implementation_Error,
			  SOME "Closed state in get_tcb")
    | get_tcb (Tcp_Tcb.Listen (tcb, _)) = tcb
    | get_tcb (Tcp_Tcb.Syn_Sent (tcb, _)) = tcb
    | get_tcb (Tcp_Tcb.Syn_Active tcb) = tcb
    | get_tcb (Tcp_Tcb.Syn_Passive (tcb, _)) = tcb
    | get_tcb (Tcp_Tcb.Estab tcb) = tcb
    | get_tcb (Tcp_Tcb.Fin_Wait_1 (tcb, _)) = tcb
    | get_tcb (Tcp_Tcb.Fin_Wait_2 tcb) = tcb
    | get_tcb (Tcp_Tcb.Close_Wait tcb) = tcb
    | get_tcb (Tcp_Tcb.Closing tcb) = tcb
    | get_tcb (Tcp_Tcb.Last_Ack tcb) = tcb
    | get_tcb (Tcp_Tcb.Time_Wait _) =
       Trace.print_raise (Tcp_State_Implementation_Error,
			  SOME "Time_Wait state in get_tcb")

(*
	12.	internal function send_ack

	Send_ack is called when the ack timer expires. We want
	send_ack to send an ack unless unacked_segs is set to 0
	which indicates we have sent an ack (usually by piggyback)
	since the last segment was received.  Also, if there are
	segments to be processed in the to_do queue, restart
	the timer instead of sending ack.
*)

  fun send_ack (Tcp_Tcb.Closed args) = Tcp_Tcb.Closed args
    | send_ack (Tcp_Tcb.Time_Wait args) = Tcp_Tcb.Time_Wait args
    | send_ack (Tcp_Tcb.Listen (tcb, max)) = Tcp_Tcb.Listen (tcb, max)
    | send_ack (Tcp_Tcb.Syn_Active tcb) = Tcp_Tcb.Syn_Active tcb
    | send_ack (Tcp_Tcb.Syn_Passive state) = Tcp_Tcb.Syn_Passive state
    | send_ack state =
       let val Tcp_Tcb.Tcb {snd_nxt, rcv_nxt, rcv_wnd, rcv_wnd_scale, ts,
			    unacked_segs, to_do, timers, ...} = get_tcb state
	   val wnd = Word32.>> (! rcv_wnd, ! rcv_wnd_scale)
	   fun no_segments_to_process NONE = true
	     | no_segments_to_process (SOME (next, Tcp_Tcb.Process_Data _)) =
	        false
	     | no_segments_to_process (SOME (next, _)) =
		no_segments_to_process (Tcp_Tcb.Q.next next)
	   fun compute_segment NONE =
	        Tcp_Tcb.Fast_Empty {seq = ! snd_nxt, ack = ! rcv_nxt,
				    wnd = wnd}
	     | compute_segment (SOME {recent, last_ack_sent}) =
		(last_ack_sent := ! rcv_nxt;
		 Tcp_Tcb.Timestamp_Empty
		   {seq = ! snd_nxt, ack = ! rcv_nxt, wnd = wnd,
		    times = {send_time = Tcp_Tcb.current_time (),
			     echo = ! recent}})
	   val no_wait = no_segments_to_process (Tcp_Tcb.Q.next (! to_do))
       in if ! unacked_segs <> 0 andalso no_wait then
	   (to_do := Tcp_Tcb.Q.add (! to_do,
				    Tcp_Tcb.Send_Segment
				      (compute_segment (! ts)));
	    unacked_segs := 0)
	  else if ! unacked_segs <> 0 then (* restart timer *)
	   (#start_ack timers) ack_time
	  else Trace.debug_constant_string "ack already sent";
	  state
       end

(*
	13.	function timeout

	When timers expire in the closed state, we ignore them.  Otherwise:

	When the user timer expires, we raise the error "user timeout".
	When the retransmit timer expires, we send the segment at the
	head of the retransmit queue and restart the timer if the
	queue is non-empty; if there are pending segments, we probe
	the send window, and otherwise we do nothing.
	When the time-wait timer expires we close the connection.
	When the ack timer expires we send an ack packet.
*)

  fun timeout (Tcp_Tcb.Closed tcb, _) = Tcp_Tcb.Closed tcb
    | timeout (state, Tcp_Tcb.User_Timer) = user_timeout (get_tcb state)
    | timeout (state, Tcp_Tcb.Resend_Timer) =
       let val tcb = get_tcb state
           val Tcp_Tcb.Tcb {to_do, srto, queued, rcv_nxt, resend, rcv_wnd,
			    rcv_wnd_scale, ts, ssthresh, cwnd, mss,
			    timers, ...} = tcb
       in case Tcp_Tcb.D.first (! resend) of
	     NONE =>
	      (to_do := Tcp_Tcb.Q.add (! to_do, Tcp_Tcb.Probe_Window);
	       state)
	   | SOME (new_resend, head) =>
(* note: RFC 1122, p. 90 suggests keeping around the un-segmented data,
   and re-segmenting as needed to retransmit. This doesn't really apply
   to our scheme of doing things. *)
	      let val (segment, time, resent) = head
	          val current_wnd = Word32.>> (! rcv_wnd, ! rcv_wnd_scale)
(* this is a re-transmission, so don't update last_ack_sent. *)
		  fun new_times () =
		       {send_time = Tcp_Tcb.current_time (),
			echo = case ! ts of
			          NONE => zero32
				| SOME {recent, ...} => ! recent}
	          val (send_out, len) =
		       case segment of
			  Tcp_Tcb.Fast_Out {seq, ack, wnd, len, data} =>
			   (Tcp_Tcb.Fast_Out {seq = seq, ack = ! rcv_nxt,
					      wnd = current_wnd, len = len,
					      data = data}, len)
			| Tcp_Tcb.Fast_Empty {seq, ack, wnd} =>
			   (Tcp_Tcb.Fast_Empty {seq = seq, ack = ! rcv_nxt,
						wnd = current_wnd}, zero)
			| Tcp_Tcb.Timestamp_Out {seq, ack, wnd, len, data,
						 times} =>
			   (Tcp_Tcb.Timestamp_Out
			      {seq = seq, ack = ! rcv_nxt, wnd = current_wnd,
			       len = len, data = data, times = new_times ()},
			    len)
			| Tcp_Tcb.Timestamp_Empty {seq, ack, wnd, times} =>
			   (Tcp_Tcb.Timestamp_Empty
			       {seq = seq, ack = ! rcv_nxt,
				wnd = current_wnd, times = new_times ()}, zero)
			| Tcp_Tcb.Out_Seg {data, seg} =>
			   let val Tcp_Tcb.Seg {seq, ack, len, wnd, up,
						options, syn_flag = s,
						fin_flag = f, reset_flag = r,
						ack_flag = a, push_flag = p,
						urgent_flag = u} = seg
			       val new_seg = 
				    Tcp_Tcb.Seg {seq = seq, ack = ! rcv_nxt,
						 len = len, wnd = ! rcv_wnd,
						 up = up,
						 options = options,
						 syn_flag = s, fin_flag = f,
						 reset_flag = r, ack_flag = a,
						 push_flag = p,
						 urgent_flag = u}
			   in (Tcp_Tcb.Out_Seg {data = data, seg = new_seg},
			       len)
			   end
		  val log = Tcp_Tcb.Log_Event
		              (Tcp_Tcb.Log.Retransmit
			       {size = Word32.toInt len})
	          val send = Tcp_Tcb.Send_Segment send_out
		  val wait = Word32.<< (! srto, 0w1) (* srto * 2 *)
		  val add = Tcp_Tcb.Q.add
		  val actions = add (add (! to_do, send), log)
		  val old_cwnd = ! cwnd
	      (* set "resent" to true for every segment in the resend queue,
	         since their send times are now useless for computing
		 the average round-trip time. *)
		  fun set_resent (s, t, _) =
                       (s, t, Tcp_Tcb.Queued_After_Resent)
		  val headless_resend = Tcp_Tcb.D.map set_resent new_resend
	      in resend := Tcp_Tcb.D.add_to_front (headless_resend,
						   (segment, time,
						    Tcp_Tcb.Resent));
	         to_do := actions;
		 (#start_resend timers) wait;
(* only do exponential backoff if this is a segment we've
   already retransmitted. RFC 1122, p. 95 *)
		 case resent of
		    Tcp_Tcb.Resent => srto := wait
		  | _ => ();
(* Van Jacobson's "Congestion Avoidance and Control", Appendix B (SIGCOMM 88)
   says: on the first retransmission timeout,
          ssthresh := ! cwnd / 2, cwnd := ! mss
   (we have scaled all quantities by MSS to give a size in bytes). *)
		 if resent = Tcp_Tcb.Never_Resent then	(* first timeout *)
		  (ssthresh := Int.quot (old_cwnd, 2);
		   cwnd := Word16.toInt (! mss);
		   to_do := add (! to_do,
				 Tcp_Tcb.Log_Event
				  (Tcp_Tcb.Log.Congestion_Window_Change
				     {new_size = ! cwnd,
				      old_size = old_cwnd})))
		 else ();
	         state
	      end (* let *)
       end (* let *)
    | timeout (state, Tcp_Tcb.Ack_Timer) = send_ack state
    | timeout (state, Tcp_Tcb.Window_Timer) =
(* RFC 1122, p. 98 (4.2.3.4), sender Silly Window Syndrome avoidance.
   "send data if it is pushed and the override timeout occurred"
   [all our data is pushed -- esb].
   "it may be convenient to combine this timer with the timer used
   to probe zero windows".
   RFC 1122, p. 92 (4.2.2.17), zero window probing
   "SHOULD send the first zero-window probe when a zero window has
   existed for the retransmission timeout period, and SHOULD increase
   exponentially the interval between successive probes."

   So if the timer expires and the window is zero, I do a window probe.
   Otherwise, I do a in-spite-of-the-silly-window push, which is also
   expressed as a probe window operation.
 *) 
       let val tcb = get_tcb state
           val Tcp_Tcb.Tcb {snd_nxt, snd_una, snd_wnd, to_do, srto, wto,
			    max_snd_wnd, timers, ...} = tcb
	   val probe = Tcp_Tcb.Probe_Window
	   val time = Word32.<< (! wto, 0w1) (* wto * 2 *)
	   val zero_window = Word32.>= ((Word32.- (! snd_nxt, ! snd_una)),
					! snd_wnd)
       in if zero_window then
	   (to_do := Tcp_Tcb.Q.add (! to_do, probe);
	    (#start_window timers) time;
	    wto := time)
	  else
	   (to_do := Tcp_Tcb.Q.add (! to_do, probe);
	    wto := ! srto;
	    max_snd_wnd := zero);
	  state
       end
    | timeout (state, Tcp_Tcb.Time_Wait_Timer) =
       (* Close the connection. *)
       (let val to_do = Tcp_Tcb.state_to_do state 
	 in to_do := Tcp_Tcb.Q.add (! to_do, Tcp_Tcb.Delete_Tcb)
	 end;
	 ((let val {to_do, timers} = Tcp_Tcb.state_basics state 
	   in Tcp_Tcb.Closed (to_do, timers)
	   end)
	    handle Tcp_Tcb.Time_Wait_State_Does_Not_Have_Basics => state))

 end (* struct *)

