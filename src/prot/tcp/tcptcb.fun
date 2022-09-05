(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edo Biagioni (esb@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

	i.	Abstract

	tcptcb.fun: the implementation for the TCP finite state machine.
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
	1.	functor Tcp_Tcb
	2.	type tcp_option and function option_string
	3.	type tcp_segment and function segment_string
	4.	types tcp_in and tcp_out, functions in_string and out_string
	5.	type tcp_timer and function timer_string
	6.	internal structure Timer_Queue
	7.	type tcp_timer_thread
	8.	function new_timer_thread
	9.	types tcp_timer_state, tcp_timer_set
	10.	internal functions new_timer, start_timer, stop_timer
	11.	function new_timer_set
	12.	type tcp_action and function action_string
	13.	exported structures
	14.	type tcp_resend_state
	15.	type tcp_tcb
	16.	function tcb_string
	17.	type tcb_state
	18.	function state_string
	19.	function state_basics
	20.	function state_to_do


	iii.	RCS Log

$Log: tcptcb.fun,v $
Revision 1.35  1997/11/19  13:50:11  cline
109.32 compatibility

Revision 1.34  97/06/04  11:47:13  esb
changed to go along with tcptcb.fun

Revision 1.33  97/04/22  11:22:54  esb
removed the optional "send" function from Send_Segment and queued.

Revision 1.32  96/12/18  17:08:49  esb
changed formatting commands so words are no longer converted to ints.

Revision 1.31  1996/10/18  20:47:43  esb
added tcp_resend_state.

Revision 1.30  1996/07/19  19:51:31  cline
*** empty log message ***

Revision 1.29  1996/07/05  17:23:54  esb
the timer was not forking a thread.

Revision 1.28  1996/06/11  03:26:45  esb
major changes in the implementation of timers, to reduce overhead.

Revision 1.27  1996/05/14  01:23:11  esb
many small changes to support Timestamp options.

Revision 1.26  1996/01/19  23:03:30  esb
adapted to the new wordarray signature.

Revision 1.25  1996/01/15  18:00:49  cline
removed FoxWord

Revision 1.24  1995/08/08  18:31:39  cline
upgraded to new signatures

Revision 1.23  1995/03/07  23:57:08  esb
updated tracing.

Revision 1.22  1995/02/09  19:51:16  esb
simplified the timers to correct a bug.

Revision 1.21  1995/02/04  21:46:53  robby
updated to 107

Revision 1.20  1995/01/18  21:15:23  esb
adapted to new COROUTINE signature.

Revision 1.19  1995/01/14  02:29:53  esb
added tcp window-scale option.

Revision 1.18  1994/10/11  15:43:04  cline
Upgraded to work with SML/NJ 1.05b

Revision 1.17  1994/08/18  20:28:10  esb
added urgent_data, peer_close, peer_reset, removed snd_up, rcv_up.

Revision 1.16  1994/08/17  16:28:26  esb
minor tuning and bug fixes.

Revision 1.15  1994/08/12  06:27:15  esb
converted to the new protocol signature.

Revision 1.14  1994/07/11  17:51:15  esb
added Fast_In for faster handling of incoming data.

Revision 1.13  1994/07/01  02:33:01  danwang
Moved control structures into Fox_Basis.

Revision 1.12  1994/06/17  22:03:30  esb
minor changes.

Revision 1.11  1994/06/16  16:42:04  danwang
Updated to use functorized Fox_Basis

Revision 1.10  1994/06/13  23:13:13  esb
moved timer implementation from tcpaction to tcptcb.

Revision 1.9  1994/05/10  08:02:06  esb
added outgoing segments Fast_Out and Fast_Push.

Revision 1.8  94/03/10  19:45:08  esb
changed messages.

Revision 1.7  94/02/17  01:07:52  esb
cleaned up logging and acking.

Revision 1.6  94/02/14  14:29:58  esb
added logging.

Revision 1.5  1994/01/30  21:01:03  esb
added Complete_Send.

Revision 1.4  1994/01/28  01:13:51  esb
added send_immediately to the TCB.

Revision 1.3  1994/01/19  21:27:44  esb
replaced the data access operations with exported structures.

Revision 1.2  1994/01/09  03:19:47  esb
first functional version.

Revision 1.1  1993/11/11  05:31:56  esb
Initial revision


	1.	functor Tcp_Tcb
*)

functor Tcp_Tcb (structure B: FOX_BASIS
		 structure Incoming: EXTERNAL
		 structure Outgoing: EXTERNAL
		 structure Tcp_Log: TCP_LOG
                   where type time = Word32.word): TCP_TCB =
 struct
  type incoming_data = Incoming.T
  type outgoing_data = Outgoing.T
  structure Log = Tcp_Log

  val n4u0 = Word32.fromInt 0
  val n4u1 = Word32.fromInt 1

(*
	2.	type tcp_option and function option_string

	[793] pp. 17-19
*)

  type time_ms = Word32.word		(* repeats every 49 days or so *)

  type timestamp = {send_time: time_ms, echo: time_ms}

(* RFC 1323, p. 21, advises a timestamp clock frequency in the range
   one Hz to one KHz.  We choose one KHz.  This is especially
   convenient since all times in the foxnet are in ms. *)

  fun current_time () =
       let val now = B.V.Time.now ()
	   val seconds = Word32.fromInt (B.V.Time.toSeconds now)
	   val ms = Word32.fromInt (B.V.Time.toMilliseconds now)
       in Word32.+ (Word32.* (seconds, 0w1000), ms)
       end

  fun timestamp_string {send_time, echo} =
       "send time " ^ Word32.fmt StringCvt.DEC send_time ^
       ", echo time " ^ Word32.fmt StringCvt.DEC echo

  datatype tcp_option = Max_Segment of {size: Word16.word}
                      | Window_Scale of {shift: Word8.word}
                      | Timestamp of timestamp

  fun option_string (Max_Segment {size}) =
       "maximum segment size " ^ Word16.fmt StringCvt.DEC size
    | option_string (Window_Scale {shift}) =
       "window scale shift " ^ Word8.fmt StringCvt.DEC shift
    | option_string (Timestamp times) =
       "timestamp " ^ timestamp_string times

(*
	3.	type tcp_segment and function segment_string
*)

  datatype tcp_segment =
      Seg of {seq: Word32.word,
	      ack: Word32.word,
	      len: Word32.word,
	      wnd: Word32.word,
	      up: Word32.word,
	      options: tcp_option list,
	      syn_flag: bool,
	      fin_flag: bool,
	      reset_flag: bool,
	      ack_flag: bool,
	      push_flag: bool,
	      urgent_flag: bool}

  fun segment_string (Seg {seq, ack, len, wnd, up, options,
			   syn_flag, fin_flag, reset_flag, ack_flag,
			   push_flag, urgent_flag}) =
   let infix --
       fun "" -- y = y
	 | x -- "" = x
	 | x -- y = x ^ "-" ^ y
       val flags = (if syn_flag then "Syn" else "") --
	           (if fin_flag then "Fin" else "") --
		   (if reset_flag then "Reset" else "") --
		   (if ack_flag then "Ack" else "") --
		   (if push_flag then "Push" else "") --
		   (if urgent_flag then "Urgent" else "")
       val dlen = if syn_flag orelse fin_flag then
	           Word32.+ (len, n4u1)
		  else len
       val end_seq = if Word32.> (dlen, n4u0) then
		      Word32.+ (seq, Word32.- (dlen, n4u1))
		     else seq
       val string_end = if Word32.> (dlen, n4u0) then
	                 "..." ^ Word32.fmt StringCvt.DEC end_seq ^
			 "(" ^ Word32.fmt StringCvt.DEC dlen ^ ")"
			else ""
       val ack_print = if ack_flag then
	                ("/" ^ Word32.fmt StringCvt.DEC ack)
		       else ""
       val window_string = if ack_flag then "+" ^ Word32.fmt StringCvt.DEC wnd
			   else ""
       fun optstring [] = ""
	 | optstring (h :: []) = " " ^ option_string h
	 | optstring (h :: t) = " " ^ option_string h ^ ";" ^ optstring t
       val opts = optstring options
   in flags ^ " " ^ Word32.fmt StringCvt.DEC seq ^
      string_end ^ ack_print ^ window_string ^ opts
   end

(*
	4.	types tcp_in and tcp_out, functions in_string and out_string
*)

  datatype tcp_in = In_Seg of {seg: tcp_segment,
			       data: incoming_data}
 (* on incoming data, we ignore the push bit. *)
                   | Fast_In of {seq: Word32.word,
				 ack: Word32.word,
				 len: Word32.word,
				 wnd: Word32.word,
				 data: incoming_data,
				 times: timestamp option}

  fun in_string (In_Seg {seg, data}) = "in: " ^ segment_string seg
    | in_string (Fast_In {seq, ack, len, wnd, data, times}) =
      "in (fast): " ^
       segment_string (Seg {seq = seq, ack = ack, len = len, wnd = wnd,
			    up = n4u0,
			    options = (case times of NONE => []
			                           | SOME x => [Timestamp x]),
			    syn_flag = false, fin_flag = false,
			    reset_flag = false, ack_flag = true,
			    push_flag = true, urgent_flag = false})

  (* Normally, the system will send out Fast_Out and Fast_Empty
     packets; Out_Segs are needed for special cases such as connection
     set-up and tear-down.  The only flags set on Fast_Out and
     Fast_Empty are ack and, for Fast_Out, push.  *)
  datatype tcp_out = Out_Seg of {seg: tcp_segment,
				 data: outgoing_data}
                   | Fast_Out of {seq: Word32.word,
				  ack: Word32.word,
				  len: Word32.word,
				  wnd: Word32.word,
				  data: outgoing_data}
                   | Fast_Empty of {seq: Word32.word,
				    ack: Word32.word,
				    wnd: Word32.word}
                   | Timestamp_Out of {seq: Word32.word,
				       ack: Word32.word,
				       len: Word32.word,
				       wnd: Word32.word,
				       data: outgoing_data,
				       times: timestamp}
                   | Timestamp_Empty of {seq: Word32.word,
					 ack: Word32.word,
					 wnd: Word32.word,
					 times: timestamp}

  fun out_string (Out_Seg {seg, data}) = "out " ^ segment_string seg
    | out_string (Fast_Out {seq, ack, len, wnd, data}) =
       "out (fast) " ^
       segment_string (Seg {seq = seq, ack = ack, len = len, wnd = wnd,
			    up = n4u0, options = [], syn_flag = false,
			    fin_flag = false, reset_flag = false,
			    ack_flag = true, push_flag = true,
			    urgent_flag = false})
    | out_string (Fast_Empty {seq, ack, wnd}) =
       "out (empty) " ^
       segment_string (Seg {seq = seq, ack = ack, len = n4u0, wnd = wnd,
			    up = n4u0, options = [], syn_flag = false,
			    fin_flag = false, reset_flag = false,
			    ack_flag = true, push_flag = false,
			    urgent_flag = false})
    | out_string (Timestamp_Out {seq, ack, len, wnd, data, times}) =
       "out (fast/timestamp) " ^
       segment_string (Seg {seq = seq, ack = ack, len = len, wnd = wnd,
			    up = n4u0, options = [Timestamp times],
			    syn_flag = false,
			    fin_flag = false, reset_flag = false,
			    ack_flag = true, push_flag = true,
			    urgent_flag = false})
    | out_string (Timestamp_Empty {seq, ack, wnd, times}) =
       "out (empty/timestamp) " ^
       segment_string (Seg {seq = seq, ack = ack, len = n4u0, wnd = wnd,
			    up = n4u0, options = [Timestamp times],
			    syn_flag = false,
			    fin_flag = false, reset_flag = false,
			    ack_flag = true, push_flag = false,
			    urgent_flag = false})

(*
	5.	type tcp_timer and function timer_string

	[793] p. 52
*)

  datatype tcp_timer =
      Resend_Timer
    | User_Timer
    | Ack_Timer
    | Window_Timer
    | Time_Wait_Timer

  fun timer_string Resend_Timer = "retransmit"
    | timer_string User_Timer = "user"
    | timer_string Ack_Timer = "ack"
    | timer_string Window_Timer = "window"
    | timer_string Time_Wait_Timer = "time-wait"

(*
	6.	internal structure Timer_Queue
*)

  type sleep = B.V.Time.time * (unit -> unit)

  fun sleep_less ((t1, _), (t2, _)) = B.V.Time.< (t1, t2)

  structure Timer_Queue = Priority_Queue (type key = sleep
					  val less = sleep_less)

(*
	7.	type tcp_timer_thread
*)

  type tcp_timer_thread = {active: bool ref, queue: Timer_Queue.T}

(*
	8.	function new_timer_thread
*)

  fun new_timer_thread () = {active = ref false, queue = Timer_Queue.new ()}

(*
	9.	types tcp_timer_state, tcp_timer_set
*)

  type tcp_timer_state = {exec: unit -> unit, stop: (unit -> unit) ref,
			  thread: tcp_timer_thread}

  type tcp_timer_set =
         {start_resend: time_ms -> unit, stop_resend: unit -> unit,
          start_user: time_ms -> unit, stop_user: unit -> unit,
          start_ack: time_ms -> unit, stop_ack: unit -> unit,
          start_window: time_ms -> unit, stop_window: unit -> unit,
          start_time_wait: time_ms -> unit, stop_time_wait: unit -> unit}

(*
	10.	internal functions new_timer, start_timer, stop_timer
*)

  local
   fun safe_delta expiration_time =
        let val now = B.V.Time.now ()
	in if B.V.Time.< (expiration_time, now) then 0
	   else let val delta = B.V.Time.- (expiration_time, now)
		in if B.V.Time.toSeconds delta > 0 then 1000
		   else B.V.Time.toMilliseconds delta
		end
	end

   fun wakeup (queue, NONE) = 0
     | wakeup (queue, SOME (time, exec)) =
        let val delta = safe_delta time
	in if delta <= 0 then	(* timer expiration *)
	    (Timer_Queue.pop queue;
	     B.Scheduler.fork exec;
	     wakeup (queue, Timer_Queue.first queue))
	   else delta
	end

   fun activate_thread (active, queue, delta) =
        (active := true;
	 B.Scheduler.sleep (if delta > 10 then 10 else delta);
	 case Timer_Queue.first queue of
	    NONE => active := false
	  | first =>
	     activate_thread (active, queue, wakeup (queue, first)))

  in
   fun new_timer (thread, exec) =
        {exec = exec, stop = ref (fn () => ()), thread = thread}

   fun start_timer {exec, stop, thread = {active, queue}} time =
	let val now = B.V.Time.now ()
	    val int_time = Word32.toInt time
	    val later = B.V.Time.+ (now, B.V.Time.fromMilliseconds int_time)
	in ((! stop ()) handle _ => ());
	   stop := Timer_Queue.add (queue, (later, exec));
	   if not (! active) then
	    (active := true;
	     B.Scheduler.fork (fn _ =>
			       activate_thread (active, queue, int_time)))
	   else ()
	end

   fun stop_timer {exec, stop, thread} = (fn _ => ((! stop ()) handle _ => ()))
  end (* local *)

(*
	11.	function new_timer_set
*)

  fun new_timer_set (thread, {resend_expiration, user_expiration,
			      ack_expiration, window_expiration,
			      time_wait_expiration}) =
       let val resend_state = new_timer (thread, resend_expiration)
           val user_state = new_timer (thread, user_expiration)
           val ack_state = new_timer (thread, ack_expiration)
           val window_state = new_timer (thread, window_expiration)
           val time_wait_state = new_timer (thread, time_wait_expiration)
       in {start_resend = start_timer resend_state,
	   stop_resend = stop_timer resend_state,
	   start_user = start_timer user_state,
	   stop_user = stop_timer user_state,
	   start_ack = start_timer ack_state,
	   stop_ack = stop_timer ack_state,
	   start_window = start_timer window_state,
	   stop_window = stop_timer window_state,
	   start_time_wait = start_timer time_wait_state,
	   stop_time_wait = stop_timer time_wait_state}
       end

(*
(*
x	12.	functions start_*_timer
*)

  fun start_timer (id, time, exec) =
       let val _ = id := ! id + 0w1
	   val id_value = ! id
	   fun loop time =
	        if time > 100 then
	         (B.Scheduler.sleep 100;
		  if ! id = id_value then loop (time - 100)
		  else ())
		else
		 (B.Scheduler.sleep time;
		  if ! id = id_value then exec () else ())
       in B.Scheduler.fork (fn _ => loop (Word32.toInt time))
       end

  fun start_resend_timer (Timer_Set {resend = (exec, id), ...}, time) =
       start_timer (id, time, exec)

  fun start_user_timer (Timer_Set {user = (exec, id), ...}, time) =
       start_timer (id, time, exec)

(* the ack timer is usually short, so we don't bother looping. *)
  fun start_ack_timer (Timer_Set {ack = (exec, id), ...}) =
       (id := ! id + 0w1;
	let val id_value = ! id
	in B.Scheduler.fork (fn _ => (B.Scheduler.sleep ack_timeout_ms;
				      if ! id = id_value then exec () else ()))
	end)

  fun start_window_timer (Timer_Set {window = (exec, id), ...}, time) =
       start_timer (id, time, exec)

(*
x	13.	function stop_timer
*)
  fun increment id = id := ! id + 0w1

  fun stop_resend_timer (Timer_Set {resend = (_, id), ...}) = increment id

  fun stop_user_timer (Timer_Set {user = (_, id), ...}) = increment id

  fun stop_ack_timer (Timer_Set {ack = (_, id), ...}) = increment id

  fun stop_window_timer (Timer_Set {window = (_, id), ...}) = increment id
*)

(*
	12.	type tcp_action and function action_string
*)

  datatype tcp_action =
      User_Error of string
    | Send_Segment of tcp_out
    | Process_Data of tcp_in
    | User_Data of incoming_data
    | Urgent_Data of incoming_data
    | Timer_Expiration of tcp_timer
    | Close_After_Sends
    | Complete_Open of bool		(* true if OK, false if error open *)
    | Complete_Close of bool		(* true if OK, false if error close *)
    | Complete_Send of Word32.word	(* ack number *)
    | Probe_Window
    | Peer_Close
    | Peer_Reset
    | Log_Event of Tcp_Log.tcp_event	(* log an event *)
    | Delete_Tcb

  fun packet_size_string packet =
       Word.fmt StringCvt.DEC (Incoming.size packet) ^ " bytes"

  fun action_string (User_Error s) = "error " ^ s
    | action_string (Send_Segment (Out_Seg {seg, data})) =
       "send " ^ segment_string seg
    | action_string (Send_Segment segment) = "send " ^ out_string segment
    | action_string (Process_Data (In_Seg {seg, data})) =
       "incoming data: " ^ packet_size_string data
    | action_string (Process_Data (Fast_In {data, ...})) =
       "incoming data: " ^ packet_size_string data
    | action_string (User_Data packet) =
       "data for user: " ^ packet_size_string packet
    | action_string (Urgent_Data packet) =
       "urgent data for user: " ^ packet_size_string packet
    | action_string (Timer_Expiration timer) =
       timer_string timer ^ " timer expiration"
    | action_string Close_After_Sends = "close after sends"
    | action_string (Complete_Open true) = "successful complete open"
    | action_string (Complete_Open false) = "unsuccessful complete open"
    | action_string (Complete_Close true) = "successful complete close"
    | action_string (Complete_Close false) = "unsuccessful complete close"
    | action_string (Complete_Send ack) =
       "complete send (" ^ Word32.fmt StringCvt.DEC ack^ ")"
    | action_string Probe_Window = "probe window"
    | action_string Peer_Close = "peer close"
    | action_string Peer_Reset = "peer reset"
    | action_string (Log_Event event) = "log " ^ Log.event_makestring event
    | action_string Delete_Tcb = "delete tcb"

(*
	13.	exported structures
*)

  structure Q = B.Fifo

  structure D = Deq (structure V = B.V)

(*
	14.	type tcp_resend_state
*)

  datatype tcp_resend_state = Never_Resent | Resent | Queued_After_Resent

  fun resend_string Never_Resent = "never resent"
    | resend_string Resent = "resent at least once"
    | resend_string Queued_After_Resent =
       "queued after a packet that was resent"

(*
	15.	type tcp_tcb

	[793] pp. 19-21
*)

  datatype tcp_tcb =
      Tcb of {iss: Word32.word,
	      snd_una: Word32.word ref,
	      snd_nxt: Word32.word ref,
	      snd_wnd: Word32.word ref,	(* amount we may send. *)
	      max_snd_wnd: Word32.word ref, (* max send window seen *)
	      snd_wl1: Word32.word ref,
	      snd_wl2: Word32.word ref,
	      snd_wnd_scale: Word.word ref,
	      irs: Word32.word ref,
	      rcv_nxt: Word32.word ref,
	      rcv_wnd: Word32.word ref,	(* amount we can receive. *)
	      rcv_sws: Word32.word ref,	(* bytes for rcv_wnd. *)
	      rcv_wnd_scale: Word.word ref,
	      wanted_wnd_scale: Word8.word,
	      ts: {recent: time_ms ref,
		   last_ack_sent: Word32.word ref} option ref,
	      send_immediately: bool ref,
	      cwnd: int ref,		(* scaled congestion window *)
	      ssthresh: int ref,	(* scaled window size threshold *)
	      unacked_segs: int ref,	(* packets received since last acked *)
	      srtt: time_ms ref,	(* scaled smoothed round-trip time *)
	      srtd: time_ms ref,	(* scaled round-trip deviation *)
	      srto: time_ms ref,	(* retransmit timeout *)
	      wto: time_ms ref,		(* window timeout *)
	      mss: Word16.word ref,		(* maximum send segment size *)
	      (* resend holds segments queued for retransmission, and
                 at what time, and whether retransmitted.  All
		 this data fits in the send window. *)
	      resend: (tcp_out * time_ms * tcp_resend_state) D.T ref,
	      (* queued holds the data which has not yet been sent. *)
	      queued: (outgoing_data * Word32.word * bool (* urg *) ) D.T ref,
	      out_of_order: tcp_in Q.T ref,	(* queued for processing *)
	      timers: tcp_timer_set,
	      to_do: tcp_action Q.T ref}

(*
	16.	function tcb_string
*)

  fun tcb_string (Tcb {iss, snd_una, snd_nxt, snd_wnd, max_snd_wnd,
		       snd_wl1, snd_wl2, snd_wnd_scale,
		       irs, rcv_nxt, rcv_wnd, rcv_sws, rcv_wnd_scale,
		       wanted_wnd_scale, ts,
		       send_immediately, cwnd, ssthresh,
		       unacked_segs, srtt, srtd, srto, wto, mss,
		       resend, queued, out_of_order, timers, to_do}) =
       "tcb: iss = " ^ Word32.fmt StringCvt.DEC iss ^ ", " ^
       "snd_una = " ^ Word32.fmt StringCvt.DEC (! snd_una) ^ ", " ^
       "snd_nxt = " ^ Word32.fmt StringCvt.DEC (! snd_nxt) ^ ", " ^
       "snd_wnd = " ^ Word32.fmt StringCvt.DEC (! snd_wnd) ^ ", " ^
       "max_snd_wnd = " ^ Word32.fmt StringCvt.DEC (! max_snd_wnd) ^ ", " ^
       "snd_wl1 = " ^ Word32.fmt StringCvt.DEC (! snd_wl1) ^ ", " ^
       "snd_wl2 = " ^ Word32.fmt StringCvt.DEC (! snd_wl2) ^ ", " ^
       "snd_wnd_scale = " ^ Word.fmt StringCvt.DEC (! snd_wnd_scale) ^ ", " ^
       "irs = " ^ Word32.fmt StringCvt.DEC (! irs) ^ ", " ^
       "rcv_nxt = " ^ Word32.fmt StringCvt.DEC (! rcv_nxt) ^ ", " ^
       "rcv_wnd = " ^ Word32.fmt StringCvt.DEC (! rcv_wnd) ^ ", " ^
       "rcv_sws = " ^ Word32.fmt StringCvt.DEC (! rcv_sws) ^ ", " ^
       "rcv_wnd_scale = " ^ Word.fmt StringCvt.DEC (! rcv_wnd_scale) ^ ", " ^
       "requested_wnd_scale = " ^ Word8.fmt StringCvt.DEC wanted_wnd_scale ^
       ", " ^
       (case ! ts of
	   NONE => "no timestamp_option, "
	 | SOME {recent, last_ack_sent} =>
	    "ts_recent = " ^ Word32.fmt StringCvt.DEC (! recent) ^ ", " ^
	    "last_ack_sent = " ^
	    Word32.fmt StringCvt.DEC (! last_ack_sent) ^ ", ") ^
       "send_immediately = " ^ Bool.toString (! send_immediately) ^ ", " ^
       "cwnd = " ^ Integer.toString (! cwnd) ^ ", " ^
       "ssthresh = " ^ Integer.toString (! ssthresh) ^ ", " ^
       "unacked_segs = " ^ Integer.toString (! unacked_segs) ^ ", " ^
       "srtt = " ^ Word32.fmt StringCvt.DEC (! srtt) ^ ", " ^
       "srtd = " ^ Word32.fmt StringCvt.DEC (! srtd) ^ ", " ^
       "srto = " ^ Word32.fmt StringCvt.DEC (! srto) ^ ", " ^
       "wto = " ^ Word32.fmt StringCvt.DEC (! wto) ^ ", " ^
       "mss = " ^ Word16.fmt StringCvt.DEC (! mss)

(*
	17.	type tcb_state

	[793] p. 21
*)

  type time_wait_state =
          {to_do: tcp_action Q.T ref,
	   restart_time_wait_timer: time_ms -> unit,
	   stop_timers: unit -> unit,
	   snd_nxt: Word32.word,
	   rcv_nxt: Word32.word}

  datatype tcp_state =
      Closed of tcp_action Q.T ref * tcp_timer_set
    | Listen of tcp_tcb * int   (* outgoing maximum segment size *)
    | Syn_Sent of tcp_tcb * int (* outgoing maximum segment size *)
    | Syn_Active of tcp_tcb	(* Syn_Received reached from active open *)
    | Syn_Passive of tcp_tcb * int (* Syn_Received reached from passive open *)
    | Estab of tcp_tcb
    | Fin_Wait_1 of tcp_tcb * bool (* bool is: fin_sent? *)
    | Fin_Wait_2 of tcp_tcb
    | Close_Wait of tcp_tcb
    | Closing of tcp_tcb
    | Last_Ack of tcp_tcb
    | Time_Wait of time_wait_state

(*
	18.	function state_string
*)

  fun state_string (Closed _) = "Closed"
    | state_string (Listen (tcb, mss)) = "Listen"
    | state_string (Syn_Sent (tcb, mss)) = "Syn_Sent"
    | state_string (Syn_Active tcb) = "Syn_Received (active open)"
    | state_string (Syn_Passive (tcb, mss)) = "Syn_Received (passive open)"
    | state_string (Estab tcb) = "Estab"
    | state_string (Fin_Wait_1 (tcb, fin_sent)) = "Fin_Wait_1"
    | state_string (Fin_Wait_2 tcb) = "Fin_Wait_2"
    | state_string (Close_Wait tcb) = "Close_Wait"
    | state_string (Closing tcb) = "Closing"
    | state_string (Last_Ack tcb) = "Last_Ack"
    | state_string (Time_Wait _) = "Time_Wait"

(*
	19.	function state_basics
*)

  exception Time_Wait_State_Does_Not_Have_Basics
  fun state_basics (Closed (actions, timers)) =
       {to_do = actions, timers = timers}
    | state_basics (Listen (Tcb {to_do, timers, ...}, _)) =
       {to_do = to_do, timers = timers}
    | state_basics (Syn_Sent (Tcb {to_do, timers, ...}, _)) =
       {to_do = to_do, timers = timers}
    | state_basics (Syn_Active (Tcb {to_do, timers, ...})) =
       {to_do = to_do, timers = timers}
    | state_basics (Syn_Passive (Tcb {to_do, timers, ...}, _)) =
       {to_do = to_do, timers = timers}
    | state_basics (Estab (Tcb {to_do, timers, ...})) =
       {to_do = to_do, timers = timers}
    | state_basics (Fin_Wait_1 (Tcb {to_do, timers, ...}, _)) =
       {to_do = to_do, timers = timers}
    | state_basics (Fin_Wait_2 (Tcb {to_do, timers, ...})) =
       {to_do = to_do, timers = timers}
    | state_basics (Close_Wait (Tcb {to_do, timers, ...})) =
       {to_do = to_do, timers = timers}
    | state_basics (Closing (Tcb {to_do, timers, ...})) =
       {to_do = to_do, timers = timers}
    | state_basics (Last_Ack (Tcb {to_do, timers, ...})) =
       {to_do = to_do, timers = timers}
    | state_basics (Time_Wait _) =
       raise Time_Wait_State_Does_Not_Have_Basics
(*
       {to_do = to_do, timers = timers}
*)

(*
	20.	function state_to_do
*)

  fun state_to_do (Closed (actions, timers)) = actions
    | state_to_do (Listen (Tcb {to_do, ...}, _)) = to_do
    | state_to_do (Syn_Sent (Tcb {to_do, ...}, _)) = to_do
    | state_to_do (Syn_Active (Tcb {to_do, ...})) = to_do
    | state_to_do (Syn_Passive (Tcb {to_do, ...}, _)) = to_do
    | state_to_do (Estab (Tcb {to_do, ...})) = to_do
    | state_to_do (Fin_Wait_1 (Tcb {to_do, ...}, _)) = to_do
    | state_to_do (Fin_Wait_2 (Tcb {to_do, ...})) = to_do
    | state_to_do (Close_Wait (Tcb {to_do, ...})) = to_do
    | state_to_do (Closing (Tcb {to_do, ...})) = to_do
    | state_to_do (Last_Ack (Tcb {to_do, ...})) = to_do
    | state_to_do (Time_Wait {to_do, ...}) = to_do

 end (* struct *)

