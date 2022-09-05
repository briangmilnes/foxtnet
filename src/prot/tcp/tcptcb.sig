(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edo Biagioni (esb@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

	i.	Abstract

	tcptcb.sig: this is the signature for the TCP finite state machine.


	ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	signature TCP_TCB


	iii.	RCS Log

$Log: tcptcb.sig,v $
Revision 1.23  1997/06/04  11:46:48  esb
changed the time_wait state to not hold on to the timers,
which would hold on to the connection state.

Revision 1.22  97/04/22  11:22:54  esb
removed the optional "send" function from Send_Segment and queued.

Revision 1.21  96/10/18  20:47:43  esb
added tcp_resend_state.

Revision 1.20  1996/06/27  20:14:51  cline
removed dashed lines

Revision 1.19  1996/06/11  03:26:35  esb
major changes in the timer interface.

Revision 1.18  1996/05/14  01:23:11  esb
many small changes to support Timestamp options.

Revision 1.17  1996/01/15  18:00:49  cline
removed FoxWord

Revision 1.16  1995/01/14  02:29:53  esb
added tcp window-scale option.

Revision 1.15  1994/10/11  15:43:04  cline
Upgraded to work with SML/NJ 1.05b

Revision 1.14  1994/08/18  20:28:10  esb
added urgent_data, peer_close, peer_reset, removed snd_up, rcv_up.

Revision 1.13  1994/08/17  16:28:26  esb
minor tuning and bug fixes.

Revision 1.12  1994/08/12  06:27:15  esb
converted to the new protocol signature.

Revision 1.11  1994/07/11  17:51:15  esb
added Fast_In for faster handling of incoming data.

Revision 1.10  1994/06/16  16:42:04  danwang
Updated to use functorized Fox_Basis

Revision 1.9  1994/06/13  23:13:13  esb
moved timer implementation from tcpaction to tcptcb.

Revision 1.8  1994/05/10  08:02:06  esb
added outgoing segments Fast_Out and Fast_Push.

Revision 1.7  94/02/17  01:07:23  esb
cleaned up logging and acking.

Revision 1.6  94/02/14  14:29:53  esb
added logging.

Revision 1.5  1994/01/30  21:00:44  esb
added Complete_Send.

Revision 1.4  1994/01/28  01:13:17  esb
added send_immediately to the TCB.

Revision 1.3  1994/01/19  21:27:44  esb
replaced the data access operations with exported structures.

Revision 1.2  1994/01/09  03:19:47  esb
first functional version.

Revision 1.1  1993/11/11  05:31:56  esb
Initial revision


	1.	signature TCP_TCB
*)

signature TCP_TCB =
 sig

  type incoming_data
  type outgoing_data

  type time_ms = Word32.word		(* repeats every 49 days or so *)

  type timestamp = {send_time: time_ms, echo: time_ms}

  val current_time: unit -> time_ms

  datatype tcp_option = Max_Segment of {size: Word16.word}
                      | Window_Scale of {shift: Word8.word}
                      | Timestamp of timestamp

  val option_string: tcp_option -> string

  datatype tcp_segment = Seg of {seq: Word32.word,
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

  val segment_string: tcp_segment -> string

  datatype tcp_in = In_Seg of {seg: tcp_segment,
			       data: incoming_data}
 (* on incoming data, we ignore the push bit. *)
                   | Fast_In of {seq: Word32.word,
				 ack: Word32.word,
				 len: Word32.word,
				 wnd: Word32.word,
				 data: incoming_data,
				 times: timestamp option}

  val in_string: tcp_in -> string

  (* Normally, the system will send out Fast_Out and Fast_Empty
     packets (or the timestamp variants if the timestamp option is
     in use); Out_Segs are needed for special cases such as connection
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

  val out_string: tcp_out -> string

  datatype tcp_timer =
      Resend_Timer
    | User_Timer
    | Ack_Timer
    | Window_Timer
    | Time_Wait_Timer

  val timer_string: tcp_timer -> string

  type tcp_timer_thread
  (* normally, one per session; goes away when g.c'd after all timers have
     expired or been stopped. *)
  val new_timer_thread: unit -> tcp_timer_thread

  type tcp_timer_set =
         {start_resend: time_ms -> unit, stop_resend: unit -> unit,
          start_user: time_ms -> unit, stop_user: unit -> unit,
          start_ack: time_ms -> unit, stop_ack: unit -> unit,
          start_window: time_ms -> unit, stop_window: unit -> unit,
          start_time_wait: time_ms -> unit, stop_time_wait: unit -> unit}
  val new_timer_set: tcp_timer_thread
                   * {resend_expiration: unit -> unit,
		      user_expiration: unit -> unit,
		      ack_expiration: unit -> unit,
		      window_expiration: unit -> unit,
		      time_wait_expiration: unit -> unit}
                   -> tcp_timer_set

  structure Log: TCP_LOG
    sharing type Log.time = time_ms

  (* a user error is an error that should be reported to the application
        or printed on the screen.
     a send segment is a request to ship the segment to the net.
     a process data is a request to process the incoming data.
     a user data is a request to deliver the data to the application.
     a set timer is a request to start a timer set to expire the given
        number of ms from now, or to extend the currently active timer
        to expire the given number of ms from now.
     a clear timer specifies that the timer should no longer be active.
     a timer expiration specifies that the timer has expired.
     a close after sends specifies that the function close should be
        called (again) after the "queued" queue becomes empty.
     a complete open specifies that the pending open may complete.
     a complete close specifies that the pending close may complete. *)
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
    | Log_Event of Log.tcp_event	(* log an event *)
    | Delete_Tcb

  val action_string: tcp_action -> string

  structure Q: FIFO
  structure D: DEQ

  datatype tcp_resend_state = Never_Resent | Resent | Queued_After_Resent

  val resend_string: tcp_resend_state -> string

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

  val tcb_string: tcp_tcb -> string

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

  val state_string: tcp_state -> string
  val state_to_do: tcp_state -> tcp_action Q.T ref
  (* This exception may be raised by state_basics *)
  exception Time_Wait_State_Does_Not_Have_Basics
  val state_basics: tcp_state
                  -> {to_do: tcp_action Q.T ref, timers: tcp_timer_set}

 end (* sig *)
