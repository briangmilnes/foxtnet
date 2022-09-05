(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edo Biagioni (esb@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

	i.	Abstract

	tcpreceive.fun: the implementation for the TCP receive operation.
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
	1.	functor Tcp_Receive
	2.	internal functions b4less and b4le
	3.	internal value empty_data
	4.	internal functions create_empty and send_empty
	5.	internal function send_ack
	6.	internal function segment_data
	7.	internal function set_window
	8.	internal function requeue_out_of_order
	9.	internal functions time_option and echo_option
	10.	internal function time_wait
	11.	internal function receive_fin
	12.	internal function queued_data
	13.	internal function has_probe
	14.	internal function process_data
	15.	internal function answer_options
	16.	internal function process_options
	17.	internal function process_syn
	18.	internal function acceptable_segment
	19.	internal function process_syn_received
	20.	internal functions complete_last_ack and answer_time_wait
	21.	internal function receive_reset
	22.	internal function receive_syn_flag
	23.	internal function receive_normal
	24.	internal function unacceptable_segment
	25.	internal function process_normal
	26.	internal function receive_syn_sent
	27.	internal function receive_listen
	28.	function receive_closed
	29.	internal function receive_time_wait
	30.	function fast_receive
	31.	function receive


	iii.	RCS Log

$Log: tcpreceive.fun,v $
Revision 1.40  1997/06/04  11:48:40  esb
changed to agree with new tcptcb.sig and to ack immediately if the
computed receive window is less than the last packet we received.

Revision 1.39  97/04/22  11:25:11  esb
adapted to new tcptcb.sig.

Revision 1.38  96/12/18  16:59:06  esb
now deletes timestamps for segments received out of order.

Revision 1.37  1996/10/18  20:52:01  esb
corrected the evaluation of when tcp segments are acceptable.

Revision 1.36  1996/06/27  20:26:46  esb
minor bug fix.

Revision 1.35  1996/06/27  18:29:23  cline
adapted to new timer interface. fixed minor bugs.

Revision 1.34  1996/06/04  19:25:56  cline
fixed printing of sequence numbers

Revision 1.33  1996/05/16  23:51:50  esb
replaced a use of Incoming.sub with Incoming.split.

Revision 1.32  1996/05/14  01:25:44  esb
changed to support timestamp options, fixed some small bugs.

Revision 1.31  1996/03/12  22:25:49  esb
adapted to new FOXWORD.

Revision 1.30  1996/01/19  23:03:30  esb
adapted to the new wordarray signature.

Revision 1.29  1996/01/15  18:00:49  cline
removed FoxWord

Revision 1.28  1995/09/25  16:49:46  cline
bug fox (process ack for unacceptable syn-ack packets)

Revision 1.27  1995/09/20  19:53:31  cline
added a comment

Revision 1.26  1995/08/08  18:31:39  cline
upgraded to new signatures

Revision 1.25  1995/03/12  17:52:02  esb
adapted to new trace.sig.

Revision 1.24  1995/03/07  23:57:08  esb
updated tracing.

Revision 1.23  1995/02/21  13:05:34  esb
upgraded for SML/NJ 1.07.

Revision 1.22  1995/02/04  21:46:48  robby
updated to 107

Revision 1.21  1995/01/14  02:28:30  esb
./tcpstateadded tcp window-scale option.

Revision 1.20  1994/10/11  15:43:04  cline
Upgraded to work with SML/NJ 1.05b

Revision 1.19  1994/08/24  22:10:48  esb
added minor optimizations and streamlining.

Revision 1.18  1994/08/18  20:30:38  esb
added urgent message support.

Revision 1.17  1994/08/17  16:28:26  esb
minor tuning and bug fixes.

Revision 1.16  1994/08/12  06:27:15  esb
converted to the new protocol signature.

Revision 1.15  1994/07/11  17:52:37  esb
optimized to use Fast_In messages.

Revision 1.14  1994/06/16  16:42:04  danwang
Updated to use functorized Fox_Basis

Revision 1.13  1994/06/13  23:13:13  esb
moved timer implementation from tcpaction to tcptcb.

Revision 1.12  1994/05/10  08:04:30  esb
one round of optimization.

Revision 1.11  94/04/06  23:18:11  esb
adapted to new receive_packet interface.

Revision 1.10  94/03/10  19:44:37  esb
changed messages.

Revision 1.9  94/03/09  21:59:10  esb
added processing of acks from unacceptable segments.

Revision 1.8  94/03/07  16:54:00  esb
no longer send an ack right away when receiving a packet.

Revision 1.7  94/02/17  01:09:35  esb
changed to send an ack for every received message.

Revision 1.6  94/01/30  20:56:57  esb
adapated to changed interface in tcpresend.

Revision 1.5  1994/01/28  01:29:24  esb
many changes, some bug fixes.

Revision 1.4  1994/01/19  21:33:14  esb
adapted to new interface, some bug fixes.

Revision 1.3  1994/01/11  00:34:37  esb
added interpretation of TCP maximum-segment-size option.

Revision 1.2  1994/01/09  03:21:21  esb
first functional version.

Revision 1.1  1993/11/11  05:31:56  esb
Initial revision


	1.	functor Tcp_Receive
*)

functor Tcp_Receive (structure Tcp_Tcb: TCP_TCB
		     structure Incoming: EXTERNAL
		     structure Outgoing: EXTERNAL
		     structure Retransmit: TCP_RETRANSMIT
		      sharing type Tcp_Tcb.tcp_state = Retransmit.Tcb.tcp_state
		          and type Tcp_Tcb.tcp_tcb = Retransmit.Tcb.tcp_tcb
  		          and type Tcp_Tcb.tcp_out = Retransmit.Tcb.tcp_out
			  and type Tcp_Tcb.incoming_data = Incoming.T
			  and type Tcp_Tcb.outgoing_data = Outgoing.T
		     val ack_time: Tcp_Tcb.time_ms
		     val time_wait_time: Tcp_Tcb.time_ms
		     structure B: FOX_BASIS
		     val debug_level: int ref option): TCP_RECEIVE =
 struct
  structure Trace = Trace (structure V = B.V
			   val debug_level = debug_level
			   val module_name = "tcpreceive.fun"
			   val makestring = fn _ => NONE)
  val local_print = Trace.local_print
  val debug_constant_string = Trace.debug_constant_string

  type tcp_state = Tcp_Tcb.tcp_state
  type tcp_in = Tcp_Tcb.tcp_in

(*
	2.	internal functions b4less and b4le

	[793] just (p. 24) says "=<" means "less than or equal"
	(modulo 2**32). A mathematician may correct me, but,
	I interpret this to mean that
		a =< b iff ((b - a + 2**32) mod 2**32) < 2**31
	In the implementation, "2**32) mod 2**32)" is achieved
	by using uWord32 operations, and the order of the operands
	for both subtraction and comparison is reversed.
*)

  val zero32 = Word32.fromInt 0
  val one32 = Word32.fromInt 1

  val signed_min_b4 = (* 0wx80000000 : Word32.word *)
        Word32.* (Word32.fromInt 0x8000, Word32.fromInt 0x10000)

  fun b4less (less, greater) =
       Word32.> (Word32.- (less, greater), signed_min_b4)

  fun b4le (less, greater) =
       Word32.< (Word32.- (greater, less), signed_min_b4)

(*
	3.	internal value empty_data

	Since this is an array with no elements, and therefore not an
	updatable data structure, it can be created once and shared for
	all uses.
*)

  val empty_data = Outgoing.uninitialized 0w0

(*
	4.	internal functions create_empty and send_empty

	Create an empty segment with the specified header fields.
*)

  fun create_empty (seq, ack_value, wnd, options, {syn, fin, rst, ack}) =
       let val seg = Tcp_Tcb.Seg {seq = seq, ack = ack_value, len = zero32,
				  wnd = wnd, up = zero32,
				  options = options,
				  syn_flag = syn, fin_flag = fin,
				  reset_flag = rst, ack_flag = ack,
				  push_flag = false, urgent_flag = false}
	   val out_seg = Tcp_Tcb.Out_Seg {seg = seg, data = empty_data}
       in (Tcp_Tcb.Send_Segment out_seg, out_seg)
       end

  fun send_empty x =
       let val (action, _) = create_empty x
       in action
       end

(*
	5.	internal function send_ack
*)

  fun send_ack (seq, ack, wnd, wnd_scale, ts) =
       let val window = Word32.>> (wnd, wnd_scale)
           val ack_packet =
	         case ts of
		    NONE => Tcp_Tcb.Fast_Empty {seq = seq, ack = ack,
						wnd = window}
		  | SOME {recent, last_ack_sent} =>
		     (last_ack_sent := ack;
		      Tcp_Tcb.Timestamp_Empty
		         {seq = seq, ack = ack, wnd = window,
			  times = {send_time = Tcp_Tcb.current_time (),
				   echo = ! recent}})
       in Tcp_Tcb.Send_Segment ack_packet
       end

(*
	6.	internal function segment_data
*)

  fun segment_data (Tcp_Tcb.In_Seg {seg, data}) = (seg, data)
    | segment_data (Tcp_Tcb.Fast_In {seq, ack, len, wnd, data, times}) =
       let val options = case times of
	                    SOME x => [Tcp_Tcb.Timestamp x]
			  | _ => []
       in (Tcp_Tcb.Seg {seq = seq, ack = ack, len = len, wnd = wnd,
			up = zero32, options = options, syn_flag = false,
			fin_flag = false, reset_flag = false,
			ack_flag = true, push_flag = true,
			urgent_flag = false},
	   data)
       end

(*
	7.	internal function set_window
*)

  fun set_window (wnd, seq, ack, snd_wnd, snd_wl1, snd_wl2,
		  max_snd_wnd, queued, to_do) =
	(* probe the window if the window is increased and
	   there is stuff queued to be sent. *)
       (if (* b4less (! snd_wnd, wnd) andalso  -- in-line b4less *)
	   Word32.> (Word32.- (! snd_wnd, wnd), signed_min_b4) andalso
	   not (Tcp_Tcb.D.empty (! queued)) then
	 (* send the queued packets. *)
	 to_do := Tcp_Tcb.Q.add (! to_do, Tcp_Tcb.Probe_Window)
	else ();
        snd_wnd := wnd;	(* RFC 793, p. 72; RFC 1122, p. 94 *)
	snd_wl1 := seq;	(* RFC 793, p. 72; RFC 1122, p. 94 *)
	snd_wl2 := ack;	(* RFC 793, p. 72; RFC 1122, p. 94 *)
	(* record the maximum send window for sender Silly Window Sindrome
	   avoidance. RFC 1122. *)
	if Word32.< (! max_snd_wnd, wnd) then
	 if Word32.< (wnd, Word32.fromInt 0x10000) then
	  max_snd_wnd := wnd
	 else ()
	else ())

(*
	8.	internal function requeue_out_of_order

	Queue for processing any segments that came in out of order.
*)

  fun requeue_out_of_order (tcb as (Tcp_Tcb.Tcb {out_of_order, to_do, ...})) =
       case Tcp_Tcb.Q.next (! out_of_order) of
	  NONE => ()
	| SOME (queue, first) =>
	   let fun delete_timestamp [] = []
		 | delete_timestamp (Tcp_Tcb.Timestamp stamp :: rest) =
	            delete_timestamp rest
		 | delete_timestamp (option :: rest) =
	            option :: delete_timestamp rest
	       fun fix_options (Tcp_Tcb.In_Seg {seg, data}) =
	            let val Tcp_Tcb.Seg {seq, ack, len, wnd, up, options,
					 syn_flag, fin_flag, reset_flag,
					 ack_flag, push_flag,
					 urgent_flag} = seg
			val new_seg = {seq = seq, ack = ack, len = len,
				       wnd = wnd, up = up,
				       options = delete_timestamp options,
				       syn_flag = syn_flag,
				       fin_flag = fin_flag,
				       reset_flag = reset_flag,
				       ack_flag = ack_flag,
				       push_flag = push_flag,
				       urgent_flag = urgent_flag}
		    in Tcp_Tcb.Process_Data (Tcp_Tcb.In_Seg
					     {seg = Tcp_Tcb.Seg new_seg,
					      data = data})
		    end
		 | fix_options (Tcp_Tcb.Fast_In {seq, ack, len, wnd,
						 data, times}) =
		    Tcp_Tcb.Process_Data (Tcp_Tcb.Fast_In
					  {seq = seq, ack = ack, len = len,
					   wnd = wnd, data = data,
					   times = NONE})
	   in to_do := Tcp_Tcb.Q.add (! to_do, fix_options first);
(*
	      let val seq = case first of
	                       Tcp_Tcb.In_Seg {seg = Tcp_Tcb.Seg {seq, ...},
					       ...} => seq
			     | Tcp_Tcb.Fast_In {seq, ...} => seq
	          val Tcp_Tcb.Tcb {irs, ...} = tcb
	          val delta = seq - ! irs
		  val delta_string = Word32.fmt StringCvt.DEC delta
		  val queue_size = Tcp_Tcb.Q.size (! out_of_order) + 1
		  val queue_string = Int.toString queue_size
	      in Trace.local_print ("reducing out_of_order to size " ^
				    queue_string ^
				    " by looking again at segment with seq " ^
				    delta_string)
	      end;
*)
	      out_of_order := queue;
	      requeue_out_of_order tcb
	   end

(*
	9.	internal functions time_option and echo_option

	Look for a timestamp option, and, if found, return the value.
*)

  fun time_option [] = NONE
    | time_option (Tcp_Tcb.Timestamp timestamps :: rest) = SOME timestamps
    | time_option (_ :: rest) = time_option rest

  fun echo_option [] = NONE
    | echo_option (Tcp_Tcb.Timestamp {send_time, echo} :: rest) = SOME echo
    | echo_option (_ :: rest) = echo_option rest

(*
	10.	internal function time_wait

	[793], p. 75

	The following, somewhat convoluted code is an attempt to
	insure that the function stop_timers stored into the Time_Wait
	state has no free pointers to the "timers" value, which would
	have pointers to the internal state of each connection.  The
	stop functions should have no such pointers.
*)

  local
   fun exec_four (f1, f2, f3, f4) () =
        (f1 ();
         f2 ();
         f3 ();
         f4 ())

  in
   fun time_wait (tcb as (Tcp_Tcb.Tcb {to_do, timers, snd_nxt,
					 rcv_nxt, ...})) =
        let val stop_timers = exec_four ((#stop_resend timers),
					 (#stop_user timers),
					 (#stop_ack timers),
					 (#stop_window timers))
        in stop_timers ();
           (#start_time_wait timers) time_wait_time;
	   to_do := Tcp_Tcb.Q.add (! to_do, Tcp_Tcb.Complete_Close true);
	   Tcp_Tcb.Time_Wait {to_do = to_do,
			      restart_time_wait_timer =
			             (#start_time_wait timers),
		              stop_timers = stop_timers,
			      snd_nxt = ! snd_nxt, rcv_nxt = ! rcv_nxt}
        end
  end

(*
	11.	internal function receive_fin

	[793], pp. 75, 76
*)

  fun receive_fin (state as (Tcp_Tcb.Closed _), _, _) = state
    | receive_fin (state as (Tcp_Tcb.Listen _), _, _) = state
    | receive_fin (state as (Tcp_Tcb.Syn_Sent _), _, _) = state
    | receive_fin (state,
		   tcb as (Tcp_Tcb.Tcb
			    {to_do, snd_nxt, snd_una, rcv_nxt, rcv_wnd,
			     rcv_wnd_scale, ts, srto, timers, ...}),
		   in_data as (Tcp_Tcb.Seg {seq, len, fin_flag, ...}, data)) =
       if fin_flag then
	let val new_rcv_nxt = Word32.+ (seq, Word32.+ (len, one32))
	    val empty = send_ack (! snd_nxt, new_rcv_nxt, ! rcv_wnd,
				  ! rcv_wnd_scale, ! ts);
	in rcv_nxt := new_rcv_nxt;
	   requeue_out_of_order tcb;
	   to_do := Tcp_Tcb.Q.add (! to_do, empty);
	   to_do := Tcp_Tcb.Q.add (! to_do, Tcp_Tcb.Peer_Close);
	   case state of
	      Tcp_Tcb.Syn_Active _ => Tcp_Tcb.Close_Wait tcb
	    | Tcp_Tcb.Syn_Passive _ => Tcp_Tcb.Close_Wait tcb
	    | Tcp_Tcb.Estab _ => Tcp_Tcb.Close_Wait tcb
	    | Tcp_Tcb.Fin_Wait_1 (_, true) => (* fin has been sent. *)
	       if ! snd_una = ! snd_nxt then (* fin acked (RFC 793, p. 75). *)
		time_wait tcb
	       else
		Tcp_Tcb.Closing tcb
	    | Tcp_Tcb.Fin_Wait_1 (_, false) => (* fin has not been sent. *)
	       (* send a fin segment, then enter the Closing state. *)
	       let val (send_action, seg) =
		         create_empty (! snd_nxt, ! rcv_nxt, ! rcv_wnd, [],
				       {syn = false, fin = true,
					rst = false, ack = true})
	       in debug_constant_string "sending fin for close";
	          to_do := Tcp_Tcb.Q.add (! to_do, send_action);
		  Retransmit.retransmit (tcb, seg, ! srto);
		  snd_nxt := Word32.+ (! snd_nxt, one32);
		  Tcp_Tcb.Closing tcb
	       end
	    | Tcp_Tcb.Fin_Wait_2 _ => time_wait tcb
	    | Tcp_Tcb.Close_Wait _ => state
	    | Tcp_Tcb.Closing _ => state
	    | Tcp_Tcb.Last_Ack _ => state
	    | Tcp_Tcb.Time_Wait {restart_time_wait_timer, ...} =>
	       (restart_time_wait_timer time_wait_time;
		state)
	    | _ => state
	end (* let *)
       else				(* fin not set *)
	state

(*
	12.	internal function queued_data
*)

  fun queued_data (Tcp_Tcb.Tcb {to_do, ...}) =
       let fun data_in_to_do NONE = false
	     | data_in_to_do (SOME (shorter_list, Tcp_Tcb.Process_Data s)) =
	        let val len = 
	                  case s of
			     Tcp_Tcb.In_Seg {data,
					     seg = Tcp_Tcb.Seg {len, ...}} =>
			      len
                           | Tcp_Tcb.Fast_In {len, ...} => len
		in if Word32.> (len, zero32) then true
		   else data_in_to_do (Tcp_Tcb.Q.next shorter_list)
		end
	     | data_in_to_do (SOME (shorter_list, _)) =
	        data_in_to_do (Tcp_Tcb.Q.next shorter_list)
       in data_in_to_do (Tcp_Tcb.Q.next (! to_do))
       end

(*
	13.	internal function has_probe
*)

  fun has_probe (Tcp_Tcb.Tcb {to_do, ...}) =
       let fun probe_in_to_do NONE = false
	     | probe_in_to_do (SOME (shorter_list,
				     Tcp_Tcb.Probe_Window)) = true
	     | probe_in_to_do (SOME (shorter_list, _)) =
	        probe_in_to_do (Tcp_Tcb.Q.next shorter_list)
       in probe_in_to_do (Tcp_Tcb.Q.next (! to_do))
       end

(*
	14.	internal function process_data

	[793], pp. 73-74; [1122], p. 93

	RFC 793, p. 74 says "this ack should be piggibacked if
	possible without incurring undue delay".

        RFC 1122, p. 96 says "ack delay MUST be less than 0.5s, and in
	a stream of full-sized packets, there SHOULD be an ack for at
	least every second segment". It also says (p. 93) that all
	queued segments MUST be processed before sending an ack.

	What we do:
	On packet receipt (this function), if the packet carries data,
        - we increment unacked_segs.
        - we start a 10ms timer -- sufficient for piggibacking if the
        reply (or window change) is immediate, and for a fast ack otherwise.
	- if the receive window is less than the packet lenght, we
	send the ack ASAP.

        On window updates (tcpmain.fun's add_to_window)
        - if unacked_segs is 2 or more, we expire the timer so the ack
        is sent immediately.

	This results in the following desirable behaviors:
	- for full speed bidirectional communication: most acks
	piggibacked.
	- for full speed unidirectional communication: ack every
	second packet.
	- for "telnet": ack and window update piggibacked on echo
	character.
	- when the window is small compared to the packet size,
	we ack every packet.
*)

  fun process_data (state,
		    tcb as (Tcp_Tcb.Tcb {snd_nxt, rcv_nxt, rcv_wnd, to_do,
					 unacked_segs, timers, ...}),
		    in_data as (Tcp_Tcb.Seg {up, urgent_flag, len, push_flag,
					     ...}, data)) =
       (if Word32.> (len, zero32) then
         (rcv_wnd := Word32.- (! rcv_wnd, Word32.min (! rcv_wnd, len));
	  rcv_nxt := Word32.+ (! rcv_nxt, len);
	  if not urgent_flag then
	   to_do := Tcp_Tcb.Q.add (! to_do, Tcp_Tcb.User_Data data)
	  else if Word32.>= (Word32.+ (up, one32), len) then
	   to_do := Tcp_Tcb.Q.add (! to_do, Tcp_Tcb.Urgent_Data data)
	  else (* only part of the data is urgent. *)
	   let val urgent_size = Word32.+ (up, one32)
	       val break = Word.fromInt (Word32.toInt urgent_size)
	       val (urgent, rest) = Incoming.split (data, break)
	   in to_do := Tcp_Tcb.Q.add (! to_do, Tcp_Tcb.Urgent_Data urgent);
	      to_do := Tcp_Tcb.Q.add (! to_do, Tcp_Tcb.User_Data rest)
	   end;
	  unacked_segs := ! unacked_segs + 1;
	  if ! rcv_wnd < len then	(* expire the ack timer immediately *)
	   (#start_ack timers) (Word32.fromInt 0)
	  else if ! unacked_segs = 1 then
	   (#start_ack timers) ack_time
	  else ();			(* timer should be running *)
(* if the length is non-zero, rcv_nxt may have advanced to where some
   of the requeued segments can be processed. We simply process
   all these segments all over again, since in the common case,
   they will be in the correct order for processing. *)
	  requeue_out_of_order tcb)
	else ();
	receive_fin (state, tcb, in_data))

(*
	15.	internal function answer_options
*)

  fun answer_options (_, _, []) = []
    | answer_options (max_size, wanted_wnd_scale,
		      Tcp_Tcb.Max_Segment {size} :: rest) =
       Tcp_Tcb.Max_Segment {size = Word16.fromInt max_size} ::
       answer_options (max_size, wanted_wnd_scale, rest)
    | answer_options (max_size, wanted_wnd_scale,
		      Tcp_Tcb.Window_Scale {shift} :: rest) =
       Tcp_Tcb.Window_Scale {shift = wanted_wnd_scale} ::
       answer_options (max_size, wanted_wnd_scale, rest)
    | answer_options (max_size, wanted_wnd_scale,
		      Tcp_Tcb.Timestamp {send_time, echo} :: rest) =
       Tcp_Tcb.Timestamp {send_time = Tcp_Tcb.current_time (),
			  echo = send_time} ::
       answer_options (max_size, wanted_wnd_scale, rest)

(*
	16.	internal function process_options
*)

  val max_window_shift = Word8.fromInt 14 (* RFC 1323, p. 11 *)

  fun process_options (tcb, [], _) = ()
    | process_options (tcb as (Tcp_Tcb.Tcb {mss, ...}),
		       (Tcp_Tcb.Max_Segment {size}) :: rest, ack) =
       (if Word16.>= (size, Word16.fromInt 60) then (* sanity check *)
	 mss := size
	else local_print ("ignoring mss option of size " ^
			  Integer.toString (Word16.toInt size));
	process_options (tcb, rest, ack))
    | process_options (tcb as (Tcp_Tcb.Tcb {snd_wnd_scale, rcv_wnd_scale,
					    wanted_wnd_scale, ...}),
		       (Tcp_Tcb.Window_Scale {shift}) :: rest, ack) =
       (if Word8.<= (shift, max_window_shift) then
	 (snd_wnd_scale := Word.fromInt (Word8.toInt shift);
	  rcv_wnd_scale := Word.fromInt (Word8.toInt wanted_wnd_scale))
	else local_print ("ignoring window scale option, scale = " ^
			  Word8.fmt StringCvt.DEC shift);
	process_options (tcb, rest, ack))
    | process_options (tcb as (Tcp_Tcb.Tcb {ts, rcv_nxt, ...}),
		       Tcp_Tcb.Timestamp {send_time, echo} :: rest, ack) =
       (case ! ts of
	   NONE =>
	    ts := SOME {recent = ref send_time, last_ack_sent = ref ack}
	 | SOME {recent, last_ack_sent} =>
	    recent := send_time;
	process_options (tcb, rest, ack))
(*
    | process_options (head :: rest) = process_options rest
*)

(*
	17.	internal function process_syn

	Do the common things that are done when passively opening
	or reopening a connection.
*)

  datatype packet_type = Listen_Packet | Regular_Packet

  fun process_syn (tcb, data, mss, process_normal,
		   Tcp_Tcb.Seg {seq, ack, len, wnd, up, options,
				syn_flag, fin_flag, reset_flag,
				ack_flag, push_flag, urgent_flag}) =
       let val Tcp_Tcb.Tcb {rcv_nxt, irs, iss, snd_nxt, snd_una,
			    to_do, rcv_wnd, wanted_wnd_scale, srto, ...} = tcb
	   val next_receive = Word32.+ (seq, one32)
	   val (action, seg) =
	          create_empty (! snd_nxt, next_receive, ! rcv_wnd,
				answer_options (mss, wanted_wnd_scale,
						options),
				{syn = true, fin = false,
				 rst = false, ack = true})
(* seq is incremented since we turn off the syn_flag *)
	   val next_receive = Word32.+ (seq, one32)
	   val fwd_seg = Tcp_Tcb.Seg {seq = next_receive, ack = ack,
				      len = len, wnd = wnd, up = up,
				      options = options,
				      syn_flag = false, fin_flag = fin_flag,
				      reset_flag = reset_flag,
				      ack_flag = false,
				      push_flag = push_flag,
				      urgent_flag = urgent_flag}
       in process_options (tcb, options, next_receive);
	  rcv_nxt := next_receive;
	  requeue_out_of_order tcb;
	  irs := seq;
	  snd_nxt := Word32.+ (! snd_nxt, one32);
	  snd_una := iss;
	  to_do := Tcp_Tcb.Q.add (! to_do, action);
	  Retransmit.retransmit (tcb, seg, ! srto);
	  process_normal (Tcp_Tcb.Syn_Passive (tcb, mss), tcb,
			  (fwd_seg, data), Listen_Packet)
       end

(*
	18.	internal function acceptable_segment

	Test for one of the four cases of an acceptable segment [793], pp. 69.
*)

  fun acceptable_segment (seg_seq, seg_len, rcv_nxt, rcv_wnd,
			  SOME {recent, last_ack_sent}, options) =
       (case time_option options of
	   NONE =>
	    acceptable_segment (seg_seq, seg_len, rcv_nxt, rcv_wnd, NONE, [])
	 | SOME {send_time, echo} =>
	    if b4le (! recent, send_time) then (* RFC 1323, p. 16, p. 35 *)
	     if b4le (seg_seq, ! last_ack_sent) andalso
	        (b4less (! last_ack_sent, Word32.+ (seg_seq, seg_len)) orelse
		 (seg_len = zero32 andalso seg_seq = ! last_ack_sent)) andalso
		acceptable_segment (seg_seq, seg_len, rcv_nxt,
				    rcv_wnd, NONE, []) then
	       (recent := send_time;
		true)
	     else
	      (Trace.debug_print (fn _ => "segment with sequence " ^
				  Word32.fmt StringCvt.DEC seg_seq ^
				  ", length " ^
				  Word32.fmt StringCvt.DEC seg_len ^
				  ", last ack sent " ^
				  Word32.fmt StringCvt.DEC (! last_ack_sent) ^
				  " cannot be used for timing");
	       acceptable_segment (seg_seq, seg_len, rcv_nxt,
				   rcv_wnd, NONE, []))
	    else
	     (* note: here RFC 1323 requires us to test whether the
		connection has been idle for more than 24 days.
		Since we don't keep track of when the last packet
		was received, we simply don't do this test, and
		allow a connection that sits idle for 24 days or
		more (and uses time-stamps) to simply lock up. *)
	     (Trace.trace_print (fn _ => "segment with recent " ^
				  Word32.fmt StringCvt.DEC (! recent) ^
				 ", send_time " ^
				  Word32.fmt StringCvt.DEC send_time ^
				 " is not acceptable ");
	      Trace.local_print ("segment with recent " ^
				  Word32.fmt StringCvt.DEC (! recent) ^
				 ", send_time " ^
				  Word32.fmt StringCvt.DEC send_time ^
				 " is not acceptable ");
	      false))
    | acceptable_segment (seg_seq, seg_len, rcv_nxt, rcv_wnd, _, _) =
       case (Word32.toInt seg_len, Word32.toInt rcv_wnd) of
	  (0, 0) => seg_seq = rcv_nxt
	| (0, _) => b4le (rcv_nxt, seg_seq) andalso
	            b4less (seg_seq, Word32.+ (rcv_nxt, rcv_wnd))
	| (_, 0) => false
	| _ =>
	   let val seg_last = Word32.- (Word32.+ (seg_seq, seg_len), one32)
	       val end_wnd = Word32.+ (rcv_nxt, rcv_wnd)
	   in (b4le (rcv_nxt, seg_seq) andalso b4less (seg_seq, end_wnd))
	      orelse
	      (b4le (rcv_nxt, seg_last) andalso b4less (seg_last, end_wnd))
	   end

(*
	19.	internal function process_syn_received
*)

  fun process_syn_received (state, tcb, in_data, to_do,
			    snd_una, snd_nxt, snd_wnd, snd_wl1, snd_wl2,
			    max_snd_wnd, queued,
			    seq, ack, wnd, options, packet_type, active) =
       if packet_type <> Listen_Packet then
	   (* RFC 793, p. 72; RFC 1122, p. 94 *)
	if b4le (! snd_una, ack) andalso b4le (ack, ! snd_nxt) then
	 (to_do := Tcp_Tcb.Q.add (! to_do, Tcp_Tcb.Complete_Open true);
	  if b4less (! snd_una, ack) then
	   (* SND.UNA < SEG.ACK <= SND.NXT *)
	   (snd_wnd := Word32.- (! snd_wnd, Word32.- (ack, ! snd_una));
	    snd_una := ack;		(* RFC 793, p. 72 *)
	    Retransmit.acknowledge (state, tcb, ack, echo_option options);
	    ())
	  else				(* duplicate ack *)
	   debug_constant_string
	      "duplicate ack in process_syn_received, ignoring";
	  set_window (wnd, seq, ack, snd_wnd, snd_wl1, snd_wl2, max_snd_wnd,
		      queued, to_do);
	  process_data (Tcp_Tcb.Estab tcb, tcb, in_data))
	else				(* ack < snd_una or snd_nxt < ack *)
	 let val action = send_empty (ack, zero32, zero32, [],
				      {syn = false, fin = false, rst = true,
				       ack = false})
	 in debug_constant_string "invalid ack reset";
	    to_do := Tcp_Tcb.Q.add (! to_do, action);
	    state
	 end
       else (* packet_type = Listen_Packet
	       RFC 793, p. 66: "... but processing of SYN and ACK
	       should not be repeated." *)
	process_data (state, tcb, in_data)

(*
	20.	internal functions complete_last_ack and answer_time_wait
*)

  fun complete_last_ack (state, to_do, timers, snd_nxt, ack) =
       if ! snd_nxt = ack then
	(to_do := Tcp_Tcb.Q.add (! to_do, Tcp_Tcb.Complete_Close true);
	 to_do := Tcp_Tcb.Q.add (! to_do, Tcp_Tcb.Delete_Tcb);
	 Tcp_Tcb.Closed (to_do, timers))
       else state

  fun answer_time_wait (state, timers: Tcp_Tcb.tcp_timer_set,
			to_do, snd_nxt, rcv_nxt) =
       (to_do := Tcp_Tcb.Q.add (! to_do,
				send_ack (! snd_nxt, ! rcv_nxt,
					  zero32, Word.fromInt 0, NONE));
	(#start_time_wait timers) time_wait_time;
	state)

(*
	21.	internal function receive_reset
*)

  fun receive_reset (state, to_do, timers) =
       case state of
	  Tcp_Tcb.Syn_Passive (tcb, max_size) =>
	   (to_do := Tcp_Tcb.Q.add (! to_do,
				    Tcp_Tcb.User_Error "connection reset");
	    to_do := Tcp_Tcb.Q.add (! to_do, Tcp_Tcb.Complete_Open false);
	    to_do := Tcp_Tcb.Q.add (! to_do, Tcp_Tcb.Peer_Reset);
	    to_do := Tcp_Tcb.Q.add (! to_do, Tcp_Tcb.Delete_Tcb);
	    Tcp_Tcb.Closed (to_do, timers))
	| Tcp_Tcb.Syn_Active (Tcp_Tcb.Tcb {to_do, timers, ...}) =>
	   (to_do := Tcp_Tcb.Q.add (! to_do, Tcp_Tcb.Peer_Reset);
	    to_do := Tcp_Tcb.Q.add (! to_do,
				    Tcp_Tcb.User_Error "connection refused");
	    to_do := Tcp_Tcb.Q.add (! to_do, Tcp_Tcb.Delete_Tcb);
	    Tcp_Tcb.Closed (to_do, timers))
	| _ =>
	   let val reset_error =
	            Tcp_Tcb.Q.add (! to_do,
				   Tcp_Tcb.User_Error "connection reset")
	       val actions =
	            case state of
		       Tcp_Tcb.Estab _ => reset_error
		     | Tcp_Tcb.Fin_Wait_1 _ => reset_error
		     | Tcp_Tcb.Fin_Wait_2 _ => reset_error
		     | Tcp_Tcb.Close_Wait _ => reset_error
		     | _ => ! to_do
	   in to_do := actions;
	      to_do := Tcp_Tcb.Q.add (! to_do, Tcp_Tcb.Peer_Reset);
	      to_do := Tcp_Tcb.Q.add (! to_do, Tcp_Tcb.Delete_Tcb);
	      Tcp_Tcb.Closed (to_do, timers)
	   end				(* else: not reset *)

(*
	22.	internal function receive_syn_flag
*)

  fun receive_syn_flag (state, tcb, to_do, timers, resend,
			segment, data, mss, process_normal) =
       case state of
	  Tcp_Tcb.Syn_Passive (tcb, max_size) =>	(* RFC 1122, p. 94 *)
	   (local_print "resetting syn_passive to listen";
	    resend := Tcp_Tcb.D.new ();
	    Tcp_Tcb.Listen (tcb, max_size))
	| _ =>
	   (to_do := Tcp_Tcb.Q.add (! to_do,
				    Tcp_Tcb.User_Error "connection reset");
	    to_do := Tcp_Tcb.Q.add (! to_do, Tcp_Tcb.Delete_Tcb);
	    Tcp_Tcb.Closed (to_do, timers))

(*
	23.	internal function receive_normal

	[793], pp. 70 ff.
*)

  fun receive_normal (state, tcb, in_data as (segment, data), packet_type) =
       let val Tcp_Tcb.Tcb {to_do, resend, mss, snd_una, snd_nxt, snd_wnd,
			    snd_wl1, snd_wl2, rcv_nxt, rcv_wnd_scale, ts,
			    max_snd_wnd, rcv_wnd, queued, timers, ...} = tcb
	   val Tcp_Tcb.Seg {seq, ack, wnd, options, ack_flag, reset_flag,
			    syn_flag, ...} = segment
       in if reset_flag then
	   receive_reset (state, to_do, timers)
	  (* here check the security and precedence -- no-op *)
          else if syn_flag andalso packet_type <> Listen_Packet then
	   receive_syn_flag (state, tcb, to_do, timers, resend,
			     segment, data, mss, process_normal)
          else if not ack_flag andalso (packet_type <> Listen_Packet) then
	   state			(* drop the segment and return. *)
          else
	   case state of
	      Tcp_Tcb.Syn_Active _ =>
	       process_syn_received (state, tcb, in_data, to_do,
				     snd_una, snd_nxt, snd_wnd, snd_wl1,
				     snd_wl2, max_snd_wnd, queued,
				     seq, ack, wnd, options, packet_type, true)
	    | Tcp_Tcb.Syn_Passive _ =>
	       process_syn_received (state, tcb, in_data, to_do,
				     snd_una, snd_nxt, snd_wnd, snd_wl1,
				     snd_wl2, max_snd_wnd, queued,
				     seq, ack, wnd, options,
				     packet_type, false)
	    | Tcp_Tcb.Last_Ack _ =>
	       complete_last_ack (state, to_do, timers, snd_nxt, ack)
	    | Tcp_Tcb.Time_Wait _ =>
	       (Trace.local_print "warning: sending an ack in final time_wait";
		answer_time_wait (state, timers, to_do, snd_nxt, rcv_nxt))
	    | _ =>		(* Estab, Fin_Wait_[12], Close_Wait, Closing *)
	       if b4le (ack, ! snd_nxt) then
	        (if b4less (! snd_una, ack) then
		  (* SND.UNA < SEG.ACK <= SND.NXT *)
		  (snd_wnd := Word32.- (! snd_wnd, Word32.- (ack, ! snd_una));
		   snd_una := ack;		(* RFC 793, p. 72 *)
		   Retransmit.acknowledge (state, tcb, ack,
					   echo_option options);
		   ())
		 else ();
	         if b4le (! snd_una, ack) andalso b4le (ack, ! snd_nxt) andalso
	            (b4less (! snd_wl1, seq) orelse
		     (! snd_wl1 = seq andalso b4le (! snd_wl2, ack))) then
		  set_window (wnd, seq, ack, snd_wnd, snd_wl1, snd_wl2,
			      max_snd_wnd, queued, to_do)
	         else ();
	         case state of
		    Tcp_Tcb.Estab _ =>
		      process_data (state, tcb, in_data)
	          | Tcp_Tcb.Fin_Wait_1 (_, true) => (* fin has been sent. *)
		     if ! snd_nxt = ! snd_una then
		        (* fin acked, go to fin-wait-2 (RFC 793, p. 73). *)
		      process_data (Tcp_Tcb.Fin_Wait_2 tcb, tcb, in_data)
		     else
		      process_data (state, tcb, in_data)
		  | Tcp_Tcb.Fin_Wait_1 (_, false) => (* fin not sent. *)
		      process_data (state, tcb, in_data)
	          | Tcp_Tcb.Fin_Wait_2 _ =>
		     ((case Tcp_Tcb.D.first (! resend) of
		          NONE => ()
		        | SOME (new_queue, head) =>
		           to_do := Tcp_Tcb.Q.add (! to_do,
						   Tcp_Tcb.Complete_Close
						      true));
		      process_data (state, tcb, in_data))
	          | Tcp_Tcb.Closing _ =>
		     if ! snd_nxt = ! snd_una then
		      receive_fin (time_wait tcb, tcb, in_data)
		     else
		      process_data (state, tcb, in_data)
	          | _ => (* Syn_Received, Close_Wait, Last_Ack, Time_Wait:
			    do not process the data, if any. *)
		     receive_fin (state, tcb, in_data))
	       else			(* b4less (! snd_nxt, ack) *)
	        (* invalid ack: ack, drop segment (RFC 793, p. 72). *)
	        (debug_constant_string "invalid ack, dropping segment";
		 to_do := Tcp_Tcb.Q.add (! to_do,
					 send_ack (! snd_nxt, ! rcv_nxt,
						   ! rcv_wnd, ! rcv_wnd_scale,
						   ! ts));
		 state)
       end (* let *)


(*
	24.	internal function unacceptable_segment

	RFC 793, p. 69
*)

  and unacceptable_segment (state, tcb, packet_type, data, ack, wnd, up,
			    options,
			    ack_flag, reset_flag, urgent_flag, fin_flag) =
       (Trace.debug_constant_string "processing unacceptable segment ";
	if reset_flag then state
	else
	 let val Tcp_Tcb.Tcb {snd_nxt, rcv_nxt, rcv_wnd, rcv_wnd_scale, ts,
			      to_do, ...} = tcb
	     val ack_packet = send_ack (! snd_nxt, ! rcv_nxt, ! rcv_wnd,
					! rcv_wnd_scale, ! ts)
	     val process_seg =
		  Tcp_Tcb.Seg {seq = ! rcv_nxt, ack = ack, len = zero32,
			       wnd = wnd, up = up, options = options,
			       syn_flag = false, fin_flag = false,
			       reset_flag = false, ack_flag = true,
			       urgent_flag = urgent_flag, push_flag = false}
	 in to_do := Tcp_Tcb.Q.add (! to_do, ack_packet);
	 (* process ack, urgent, and window information. *)
	     if ack_flag andalso not fin_flag then
	      receive_normal (state, tcb, (process_seg, data), packet_type)
	     else
	      state
	 end)

(*
	25.	internal function process_normal

	[793], pp. 69 and 70
*)

  and process_normal (state, tcb, in_data as (seg, data), packet_type) =
       let val Tcp_Tcb.Seg {seq, ack, len, wnd, up, options, 
			    syn_flag, fin_flag, reset_flag, ack_flag,
			    urgent_flag, push_flag} = seg
	   val Tcp_Tcb.Tcb {snd_nxt, rcv_nxt, rcv_wnd, rcv_wnd_scale, snd_wnd,
			    ts, out_of_order, to_do, ...} = tcb
	   val adj_length = if fin_flag orelse syn_flag then
	                     Word32.+ (len, one32)
			    else len
       in if acceptable_segment (seq, adj_length, ! rcv_nxt, ! rcv_wnd,
				 ! ts, options) then
	   (* segment is ok: trim if necessary. *)
	   let val expected_seg = ! rcv_nxt
	       val seg_last = Word32.- (Word32.+ (seq, adj_length), one32)
	       val end_wnd = Word32.+ (expected_seg, ! rcv_wnd)
     (* logically, new_end = Word32.min (seg_last,
                                            Word32.- (end_wnd, 0w1)).
        To use modulo-(2^32) arithmetic, we must use b4less. *)
	       val new_end = if b4less (seg_last, end_wnd) then seg_last
			     else Word32.- (end_wnd, one32)
	   in if expected_seg = seq andalso new_end = seg_last then
	    (* common, fast case *)
	       receive_normal (state, tcb, in_data, packet_type)
	      else if b4le (seq, ! rcv_nxt) then (* trim packet. *)
	       let val skip = Word.fromInt (Word32.toInt
					    (Word32.- (expected_seg, seq)))
		   val adj_skip = if syn_flag then skip - 0w1 else skip
		   val trailer = Word.fromInt (Word32.toInt
					       (Word32.- (seg_last, new_end)))
		   val adj_trailer = if fin_flag then trailer - 0w1
				     else trailer
		   val len = Incoming.size data - adj_skip - adj_trailer
		   val (_, adj_data) = Incoming.split (data, adj_skip)
		   val (short_data, _) = Incoming.split (adj_data, len)
		   val new_len = Word32.fromInt (Word.toInt len)
		   val new_syn = syn_flag andalso expected_seg = seq
		   val new_fin = fin_flag andalso new_end = seg_last
		   val new_seg = Tcp_Tcb.Seg {seq = expected_seg, ack = ack,
					      len = new_len, wnd = wnd,
					      up = up, options = options,
					      syn_flag = new_syn,
					      fin_flag = new_fin,
					      reset_flag = reset_flag,
					      ack_flag = ack_flag,
					      urgent_flag = urgent_flag,
					      push_flag = push_flag}
	       in Trace.debug_print (fn _ =>
				     "segment endpoints shrunk, were " ^
				     Word32.fmt StringCvt.DEC seq ^ "..." ^
				     Word32.fmt StringCvt.DEC seg_last ^
				     ", now are " ^
				     Word32.fmt StringCvt.DEC expected_seg ^
				     "..." ^
				     Word32.fmt StringCvt.DEC new_end);
		  receive_normal (state, tcb, (new_seg, short_data),
				  packet_type)
	       end (* let *)
	      else	(* missing an earlier segment, queue this one. *)
	       (if Word32.> (len, zero32) then
		 (debug_constant_string "queueing out-of-order segment";
(*
		  let val Tcp_Tcb.Tcb {irs, ...} = tcb
		      val delta = seq - ! irs
		      val delta_string = Word32.fmt StringCvt.DEC delta
		      val queue_size = Tcp_Tcb.Q.size (! out_of_order) + 1
		      val queue_string = Int.toString queue_size
		  in Trace.local_print ("increasing out_of_order to size " ^
					queue_string ^
					" by queueing segment with seq " ^
					delta_string)
		  end;
*)
		  out_of_order :=
		     Tcp_Tcb.Q.add (! out_of_order,
				    Tcp_Tcb.In_Seg {seg = seg, data = data}))
		else ();		(* don't queue empty segments *)
	 (* process ack, urgent, and window information. *)
		if ack_flag andalso not reset_flag
		   andalso not syn_flag andalso not fin_flag then
		 let val process_seg =
		           Tcp_Tcb.Seg {seq = ! rcv_nxt, ack = ack,
					len = zero32, wnd = wnd, up = up,
					options = options,
					syn_flag = false, fin_flag = false,
					reset_flag = false, ack_flag = true,
					urgent_flag = urgent_flag,
					push_flag = push_flag}
		 in receive_normal (state, tcb, (process_seg, data),
				    packet_type)
		 end
		else state)
	   end (* let *)
	  else	(* unacceptable segment: RFC 793, p. 69 *)
	   (
(*
            let val Tcp_Tcb.Tcb {irs, ...} = tcb
		val delta = seq - ! irs
		val delta_rcv = ! rcv_nxt - ! irs
		val opt_string = foldr (fn (a, b) => a ^ ", " ^ b) ""
		                 (map Tcp_Tcb.option_string options)
		val ts_string = case ! ts of
		                   NONE => "NONE"
				 | SOME {recent, last_ack_sent} =>
				    "(recent = " ^
				    Word32.fmt StringCvt.DEC (! recent) ^
				    ", last ack sent = " ^
				    Word32.fmt StringCvt.DEC
				      (! last_ack_sent - ! irs) ^
				    ")"
	    in Trace.local_print ("unacceptable segment, seq = " ^
				  Word32.fmt StringCvt.DEC delta ^
				  ", length = " ^
				  Word32.fmt StringCvt.DEC adj_length ^
				  ", rcv_nxt = " ^
				  Word32.fmt StringCvt.DEC delta_rcv ^
				  ", rcv_wnd = " ^
				  Word32.fmt StringCvt.DEC (! rcv_wnd) ^
				  ", options = " ^ opt_string ^
				  "ts = " ^ ts_string)
	    end;
*)
	    unacceptable_segment (state, tcb, packet_type, data, ack, wnd, up,
				  options, ack_flag, reset_flag, urgent_flag,
				  fin_flag))
       end (* let *)

(*
	26.	internal function receive_syn_sent

	What to do with a packet received in state Syn_Sent
	[793], pp. 66-68
*)

  fun receive_syn_sent (tcb as (Tcp_Tcb.Tcb {iss, snd_una, snd_nxt, mss, srto,
					     queued, timers, to_do, ...}),
			max_size,
			(seg as (Tcp_Tcb.Seg {ack_flag, reset_flag, ack,
					      syn_flag, options, ...}),
			 data)) =
       if ack_flag andalso
	  (b4le (ack, iss) orelse (b4less (! snd_nxt, ack))) then
	if reset_flag then		(* drop the packet *)
	 Tcp_Tcb.Syn_Sent (tcb, max_size)
	else				(* send a reset *)
	 (debug_constant_string "invalid ack value reset";
	  to_do := Tcp_Tcb.Q.add (! to_do, 
				  send_empty (ack, zero32, zero32, [],
					      {syn = false, fin = false,
					       rst = true, ack = false}));
	  Tcp_Tcb.Syn_Sent (tcb, max_size))
       else if reset_flag then		(* ack acceptable or not present *)
	if ack_flag then
	 (to_do := Tcp_Tcb.Q.add (! to_do, Tcp_Tcb.Peer_Reset);
	  to_do := Tcp_Tcb.Q.add (! to_do,
				  Tcp_Tcb.User_Error "connection reset");
	  to_do := Tcp_Tcb.Q.add (! to_do, Tcp_Tcb.Delete_Tcb);
	  Tcp_Tcb.Closed (to_do, timers))
	else
	 Tcp_Tcb.Syn_Sent (tcb, max_size)
	(* here check the security and precedence -- no-op *)
       else if syn_flag then		(* not reset *)
	let val Tcp_Tcb.Seg {seq, ack, wnd, fin_flag, push_flag,
			     urgent_flag, options, ...} = seg
	    val Tcp_Tcb.Tcb {iss, snd_una, resend, snd_nxt, snd_wnd,
			     max_snd_wnd, snd_wl1, snd_wl2, wanted_wnd_scale,
			     irs, rcv_nxt, rcv_wnd,
			     srtt, srto, mss,
			     queued, to_do, ...} = tcb
	    val next_receive = Word32.+ (seq, one32)
	    val estab_action = send_empty (! snd_nxt, next_receive,
					   ! rcv_wnd, [],
					   {syn = false, fin = false,
					    rst = false, ack = true})
	    val (syn_rcv_action, syn_rcv_seg) =
	           create_empty (iss, next_receive, ! rcv_wnd,
				 answer_options (max_size, wanted_wnd_scale,
						 options),
				 {syn = true, fin = false,
				  rst = false, ack = true})
	in process_options (tcb, options, next_receive);
	   rcv_nxt := next_receive;
	   requeue_out_of_order tcb;
	   irs := seq;
	   if ack_flag then
	    (snd_wnd := Word32.- (! snd_wnd, Word32.- (ack, ! snd_una));
	     snd_una := ack;
	     Retransmit.acknowledge (Tcp_Tcb.Syn_Sent (tcb, max_size),
				     tcb, ack, echo_option options);
	     ())
	   else ();
	   if b4less (iss, ! snd_una) then (* enter Estab *)
	    (set_window (wnd, seq, ack, snd_wnd, snd_wl1, snd_wl2,
			 max_snd_wnd, queued, to_do);
	     to_do := Tcp_Tcb.Q.add (! to_do, estab_action);
	     to_do := Tcp_Tcb.Q.add (! to_do, Tcp_Tcb.Complete_Open true);
	     if fin_flag orelse push_flag orelse urgent_flag orelse
	        Incoming.size data > 0w0 then
	      process_data (Tcp_Tcb.Estab tcb, tcb, (seg, data))
	     else			(* return *)
	      Tcp_Tcb.Estab tcb)
	   else				(* enter syn_received *)
	    (to_do := Tcp_Tcb.Q.add (! to_do, syn_rcv_action);
	     Retransmit.retransmit (tcb, syn_rcv_seg, ! srto);
	     if fin_flag orelse push_flag orelse urgent_flag orelse
	        Incoming.size data > 0w0 then
	      process_data (Tcp_Tcb.Syn_Active tcb, tcb, (seg, data))
	     else			(* return *)
	      Tcp_Tcb.Syn_Active tcb)
	end
       else (* neither syn nor rst set, so drop segment *)
	Tcp_Tcb.Syn_Sent (tcb, max_size)

(*
	27.	internal function receive_listen

	[793], pp. 65-66; [1122], p. 93
*)

  fun receive_listen (tcb, mss, 
		      (segment as (Tcp_Tcb.Seg
				   {seq, ack, len, wnd, up, options,
				    syn_flag, fin_flag, reset_flag, ack_flag,
				    push_flag, urgent_flag}), data)) =
       if reset_flag then Tcp_Tcb.Listen (tcb, mss)
       else if ack_flag then
	let val Tcp_Tcb.Tcb {to_do, ...} = tcb
	    val action = send_empty (ack, zero32, zero32, [],
				     {syn = false, fin = false,
				      rst = true, ack = false})
	in debug_constant_string "invalid ack seen, reset";
	   to_do := Tcp_Tcb.Q.add (! to_do, action);
	   Tcp_Tcb.Listen (tcb, mss)
	end
       else if syn_flag then
	(* here check the security and precedence -- no-op *)
	process_syn (tcb, data, mss, process_normal, segment)
       else			(* no syn, no ack: drop the segment *)
        Tcp_Tcb.Listen (tcb, mss)

(*
	28.	function receive_closed
*)

  fun receive_closed (to_do, timers, (seg, data)) =
       let val Tcp_Tcb.Seg {seq, ack, len, syn_flag, fin_flag,
			    reset_flag, ack_flag, ...} = seg
           val new_seq = if ack_flag then ack else zero32
           val new_ack = Word32.+ (Word32.+ (seq, len),
			          if syn_flag orelse fin_flag then one32
				  else zero32)
	   val action = send_empty (new_seq, new_ack, zero32, [],
				    {syn = false, fin = false, rst = true,
				     ack = not ack_flag})
       in if not reset_flag then
	   (debug_constant_string "closed receive, reset";
	    to_do := Tcp_Tcb.Q.add (! to_do, action))
	  else ();
	  Tcp_Tcb.Closed (to_do, timers)
       end

(*
	29.	internal function receive_time_wait

	note: RFC 1122 (p. 88) allows, but does not require,
	re-opening a connection which is in state time-wait on receipt
	of a SYN packet.  This is not implemented.  Instead, we send a
	RST, as specified by RFC 793, p. 71
*)

  fun receive_time_wait (restart_timer, snd_nxt, rcv_nxt, in_data) =
       let val (seq, ack, len, syn_flag, fin_flag, ack_flag, reset_flag, opt) =
	         case in_data of
		    Tcp_Tcb.In_Seg {seg =
				    Tcp_Tcb.Seg {seq, ack, len, syn_flag,
						 fin_flag, ack_flag,
						 reset_flag, options, ...},
				    ...} =>
		     (seq, ack, len, syn_flag, fin_flag, ack_flag,
		      reset_flag, options)
		  | Tcp_Tcb.Fast_In {seq, ack, len, ...} =>
		     (seq, ack, len, false, false, true, false, [])
	   val adj_length = if fin_flag orelse syn_flag then
	                     Word32.+ (len, one32)
			    else len
       in if acceptable_segment (seq, adj_length, rcv_nxt, one32,
				 NONE, opt) then
	   if syn_flag then    (* RFC 793, p. 71 *)
	    (debug_constant_string "refusing to re-open time-wait connection";
	     SOME (send_empty (ack, seq, zero32, [],
			       {syn = false, fin = false,
				rst = true, ack = false})))
	    else if ack_flag then
	     (restart_timer time_wait_time;
	      SOME (send_ack (snd_nxt, rcv_nxt, one32, 0w0, NONE)))
	    else NONE			(* RFC 793 p. 72, ignore. *)
	  else				(* unacceptable segment *)
	   if reset_flag then NONE
	   else
	    SOME (send_ack (snd_nxt, rcv_nxt, one32, 0w0, NONE))
       end

(*
	30.	function fast_receive

	This function is called in state Estab.  If ack is set and
	syn, fin, reset, and urg are clear, and if the segment
	sequence number is rcv_nxt and the segment length is no
	greater than rcv_wnd, then fast_receive queues any data for
	delivery to the user and updates the appropriate fields.  If
	the conditions do not hold, we call process_normal.
*)

  fun fast_receive (tcb as (Tcp_Tcb.Tcb {snd_nxt, rcv_nxt, rcv_wnd, ts,
					 snd_wnd_scale, ...}),
		    input as (Tcp_Tcb.Fast_In {seq, ack, len, wnd, data,
					       times})) =
       if ! rcv_nxt = seq andalso
	  Word32.<= (len, ! rcv_wnd) andalso
	  (* in-line of b4le (ack, ! snd_nxt) *)
	  Word32.< (Word32.- (! snd_nxt, ack), signed_min_b4) andalso
	  (case (! ts, times) of
	      (SOME {recent, last_ack_sent}, SOME {send_time, echo}) =>
	       if b4le (! recent, send_time) then (* RFC 1323, p. 35, common *)
		(if ! last_ack_sent = seq then recent := send_time else ();
		 true)
	       else false
	    | _ => true) then
	let val Tcp_Tcb.Tcb {snd_una, snd_wnd, ...} = tcb
	    val new_ack = Word32.- (ack, ! snd_una)
	    val seg_wnd = Word32.<< (wnd, ! snd_wnd_scale)
	in (* modified in-line of b4less (! snd_una, ack):
	      the first if tests b4le (! snd_una, ack), the
	      "if new_ack <> zero32" tests b4less (! snd_una, ack) *)
           if Word32.< (new_ack, signed_min_b4) then
	    (* SND.UNA <= SEG.ACK <= SND.NXT *)
	    (if new_ack <> zero32 then	(* SND.UNA < SEG.ACK <= SND.NXT *)
	      (snd_wnd := Word32.- (! snd_wnd, new_ack);
	       snd_una := ack;		(* RFC 793, p. 72 *)
	       Retransmit.acknowledge (Tcp_Tcb.Estab tcb, tcb, ack,
				       case times of
					  SOME {send_time, echo} => SOME echo
					| _ => NONE);
	       ())
	     else ();
	     let val Tcp_Tcb.Tcb {snd_wl1, snd_wl2, ...} = tcb
	     (* in-line b4less in: if b4less (! snd_wl1, seq) orelse *)
	     in if Word32.> (Word32.- (! snd_wl1, seq),
			     signed_min_b4) orelse
	           (! snd_wl1 = seq andalso
		    (* in-line b4le in: b4le (! snd_wl2, ack) *)
		    Word32.< (Word32.- (ack, ! snd_wl2),
				 signed_min_b4)) then
		   (* in-line call to set_window *)
		   let val Tcp_Tcb.Tcb {max_snd_wnd, queued, to_do, ...} = tcb
		   in (* if (* b4less (! snd_wnd, wnd) -- inline b4less *)
			 Word32.> (Word32.- (! snd_wnd, seg_wnd),
				      signed_min_b4)
			 andalso not (Tcp_Tcb.D.empty (! queued)) then *)
			 (* send the queued packets. *)
		      to_do := Tcp_Tcb.Q.add (! to_do, Tcp_Tcb.Probe_Window);
		      (* else (); *)
		      snd_wnd := seg_wnd;(* RFC 793, p. 72; RFC 1122, p. 94 *)
		      snd_wl1 := seq;	(* RFC 793, p. 72; RFC 1122, p. 94 *)
		      snd_wl2 := ack;	(* RFC 793, p. 72; RFC 1122, p. 94 *)
		      (* record the maximum send window for sender Silly
		         Window Sindrome avoidance. RFC 1122. *)
		      if Word32.< (! max_snd_wnd, seg_wnd) then
		       max_snd_wnd := seg_wnd
		      else ()
		   end
		else ()			(* old packet, do not update window *)
	     end)
	   else ();
	   (* note that len <= ! rcv_wnd *)
	   rcv_wnd := Word32.- (! rcv_wnd, len);
	   if Word32.> (len, zero32) then
	    (rcv_nxt := Word32.+ (! rcv_nxt, len);
	     let val Tcp_Tcb.Tcb {to_do, unacked_segs, timers,
				  out_of_order, ...} = tcb
	     in to_do := Tcp_Tcb.Q.add (! to_do, Tcp_Tcb.User_Data data);
	        unacked_segs := ! unacked_segs + 1;
	        if ! unacked_segs = 1 then (#start_ack timers) ack_time
		else ();
	        case Tcp_Tcb.Q.next (! out_of_order) of
	           NONE => ()		(* fast path. *)
	         | _ => requeue_out_of_order tcb
	     end)
	   else ();			(* length = 0 *)
	   Tcp_Tcb.Estab tcb		(* return value *)
	end
       else				(* not fast path processing *)
	process_normal (Tcp_Tcb.Estab tcb, tcb, segment_data input,
			Regular_Packet)
    | fast_receive (tcb, input as (Tcp_Tcb.In_Seg {seg, data})) =
       process_normal (Tcp_Tcb.Estab tcb, tcb, (seg, data), Regular_Packet)

(*
	31.	function receive

	[793], pp. 65 ff.
*)

  fun receive (Tcp_Tcb.Estab tcb, data) = fast_receive (tcb, data)
    | receive (Tcp_Tcb.Closed (to_do, timers), input) =
       receive_closed (to_do, timers, segment_data input)
    | receive (state as (Tcp_Tcb.Close_Wait tcb), input) =
       process_normal (state, tcb, segment_data input, Regular_Packet)
    | receive (state as (Tcp_Tcb.Fin_Wait_1 (tcb, fin_sent)), input) =
       process_normal (state, tcb, segment_data input, Regular_Packet)
    | receive (state as (Tcp_Tcb.Fin_Wait_2 tcb), input) =
       process_normal (state, tcb, segment_data input, Regular_Packet)
    | receive (Tcp_Tcb.Listen (tcb, mss), input) =
       receive_listen (tcb, mss, segment_data input)
    | receive (Tcp_Tcb.Syn_Sent (tcb, mss), input) =
       receive_syn_sent (tcb, mss, segment_data input)
    | receive (state as (Tcp_Tcb.Syn_Active tcb), input) =
       process_normal (state, tcb, segment_data input, Regular_Packet)
    | receive (state as (Tcp_Tcb.Syn_Passive (tcb, max_size)), input) =
       process_normal (state, tcb, segment_data input, Regular_Packet)
    | receive (state as (Tcp_Tcb.Closing tcb), input) =
       process_normal (state, tcb, segment_data input, Regular_Packet)
    | receive (state as (Tcp_Tcb.Last_Ack tcb), input) =
       process_normal (state, tcb, segment_data input, Regular_Packet)
    | receive (state as (Tcp_Tcb.Time_Wait {to_do, restart_time_wait_timer,
					    stop_timers, snd_nxt, rcv_nxt}),
	       input) =
       (case receive_time_wait (restart_time_wait_timer,
				snd_nxt, rcv_nxt, input) of
	   NONE => ()
	 | SOME element => to_do := Tcp_Tcb.Q.add (! to_do, element);
	state)

 end (* struct *)

