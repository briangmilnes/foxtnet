(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edo Biagioni (esb@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

	i.	Abstract

	Care and feeding of tcp's retransmit queue.

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
	1.	functor Tcp_Retransmit
	2.	function retransmit
	3.	internal function b4less
	4.	internal function recompute_srtt
	5.	internal function recompute_cwnd
	6.	function acknowledge


	iii.	RCS Log

$Log: tcpresend.fun,v $
Revision 1.31  1997/03/04  17:33:59  derby
Fixed the "0x80000000" constant.

Revision 1.30  1997/01/24  14:59:40  cline
Replaced quot with Int.quot

Revision 1.29  1996/12/18  17:03:49  esb
minor improvements.

Revision 1.28  1996/10/23  20:50:54  esb
had mis-named a variable.

Revision 1.27  1996/10/23  20:50:07  esb
somebody (RCS? AFS?) lost the last 10% of the file.

Revision 1.26  1996/10/23  20:25:16  esb
now restarts the resend timer whenever at least one segment is acked.

Revision 1.25  1996/10/18  20:51:02  esb
adapted to new tcptcb/tcp_resent_state.

Revision 1.24  1996/06/27  20:17:58  cline
cosmetic changes

Revision 1.23  1996/06/11  03:31:04  esb
adapted to new timer interface.

Revision 1.22  1996/05/16  23:52:16  esb
introduced a more conservative estimate for timeout, leading
to higher performance in the cases tested.

Revision 1.21  1996/05/14  01:25:25  esb
changed to support Timestamp options, simplified.

Revision 1.20  1996/04/18  21:22:36  cline
updated to match new TIME signature

Revision 1.19  1996/01/15  18:00:49  cline
removed FoxWord

Revision 1.18  1995/08/08  18:31:39  cline
upgraded to new signatures

Revision 1.17  1995/03/10  03:47:37  esb
adapted to new vendor.sig.

Revision 1.16  1995/03/07  23:57:08  esb
updated tracing.

Revision 1.15  1995/02/04  21:46:49  robby
updated to 107

Revision 1.14  1994/10/11  15:43:04  cline
Upgraded to work with SML/NJ 1.05b

Revision 1.13  1994/08/24  22:10:48  esb
added minor optimizations and streamlining.

Revision 1.12  1994/08/18  20:30:03  esb
added urgent message support.

Revision 1.11  1994/08/17  16:28:26  esb
minor tuning and bug fixes.

Revision 1.10  1994/08/12  06:27:15  esb
converted to the new protocol signature.

Revision 1.9  1994/07/07  02:29:37  esb
added a check to make sure cwnd doesn't grow without bounds.

Revision 1.8  1994/06/13  23:13:13  esb
moved timer implementation from tcpaction to tcptcb.

Revision 1.7  1994/05/10  08:03:24  esb
minor optimizations.

Revision 1.6  94/03/25  16:21:11  esb
placed a limit on the maximum size of the congestion window.

Revision 1.5  94/02/17  01:08:14  esb
the logged values are no longer scaled.

Revision 1.4  94/02/14  14:27:39  esb
added logging of congestion window changes.

Revision 1.3  1994/01/30  20:58:57  esb
added support for TCP slow start and for send returning after data is acked.

Revision 1.2  1994/01/28  01:28:03  esb
implemented Karn algorithm, van Jacobson's slow start and rtt computation.

Revision 1.1  1994/01/20  19:29:29  esb
Initial revision


	1.	functor Tcp_Retransmit
*)

functor Tcp_Retransmit (structure Tcp_Tcb: TCP_TCB
			structure B: FOX_BASIS): TCP_RETRANSMIT =
 struct

  structure Tcb = Tcp_Tcb

  val one32 = Word32.fromInt 1
  val zero32 = Word32.fromInt 0

(*
	2.	function retransmit
*)

  fun retransmit (tcb as (Tcp_Tcb.Tcb {resend, to_do, timers, ...}),
		  out_seg, timeout) =
       let val send_time = Tcp_Tcb.current_time ()
	   val resent = Tcp_Tcb.Never_Resent
	   val record = (out_seg, send_time, resent)
       in if Tcp_Tcb.D.empty (! resend) then
	   (#start_resend timers) timeout
	  else ();
	  resend := Tcp_Tcb.D.add_to_back (! resend, record);
	  tcb
       end

(*
	3.	internal function b4less
*)

  local

  val signed_min_b4 = (* 0wx80000000 : Word32.word *)
        Word32.* (Word32.fromInt 0x8000, Word32.fromInt 0x10000)

   fun b4less (less, greater) =
        Word32.> (Word32.- (less, greater), signed_min_b4)

(*
	4.	internal function recompute_srtt

	In Jacobson's description, M is the time since sent (we use
        delta), SA is the scaled average (srtt), SD is the scaled
        deviation (srtd), rto is the retransmit timeout (srto).
*)

   fun recompute_srtt (start, tcb) =
        let val Tcp_Tcb.Tcb {srtt, srtd, to_do, srto, ...} = tcb
	    val deltams = Word32.- (Tcp_Tcb.current_time (), start)
	    val old_srtt = ! srtt
	    val old_rtt = Word32.>> (old_srtt, 0w3) (* old_srtt quot 8 *)
	    val centered_delta = deltams - old_rtt
	    val abs_delta = if centered_delta > signed_min_b4 then
	                     Word32.- (zero32, centered_delta)
			    else centered_delta
	    val new_srtt = old_srtt + centered_delta
	    val old_srtd = ! srtd
	    val new_srtd = Word32.- (Word32.+ (old_srtd, abs_delta),
				     Word32.>> (old_srtd, 0w2))
	                                     (* old_srtd quot 4 *)
	    val new_rtt = Word32.>> (new_srtt, 0w3) (* new_srtt quot 8 *)
(* the second computation for new_srto, below, is the standard.  However,
   preliminary tests show this one to have better throughput. *)
	    val new_srto = Word32.+ (Word32.>> (new_srtt, 0w2), new_srtd)
(*
	    val new_srto = Word32.>> (Word32.+ (Word32.>> (new_srtt, 0w2),
						new_srtd), 0w1)
*)
	    val log = Tcp_Tcb.Log_Event
	               (Tcp_Tcb.Log.Round_Trip_Time_Change
			{new_rtt = new_rtt, old_rtt = old_rtt,
			 new_rto = new_srto, old_rto = ! srto})
	    val toString = Word32.toString
	in srtt := new_srtt;
	   srtd := new_srtd;
	   srto := new_srto;
	   to_do := Tcp_Tcb.Q.add (! to_do, log)
	end

(*
	5.	internal function recompute_cwnd

	Note that all quantities are multiplied by mss compared to
        Jacobson's description (appendix B of Congestion Avoidance and
        Control, SIGCOMM 88). This gives us sizes in bytes rather than
        in units of MSS, and also helps the integer division produce
        the desired results.  For example, the last case of delta is
        really 1/cwnd, since mss^2/cwnd = mss/(cwnd[*mss]/mss) =
        mss(1/cwnd).
*)

   fun recompute_cwnd (Tcp_Tcb.Tcb {cwnd, ssthresh, mss, to_do, ...}) =
        let val int_mss = Word16.toInt (! mss)
	    val int_cwnd = ! cwnd
	    val delta = if int_cwnd < ! ssthresh then int_mss
			else Int.quot (int_mss * int_mss, int_cwnd)
	    val new_cwnd = int_cwnd + delta
	    val log_data = {new_size = new_cwnd, old_size = int_cwnd}
	    val log = Tcp_Tcb.Log.Congestion_Window_Change log_data
	in cwnd := (if new_cwnd < 0xffffff then new_cwnd else 0xffffff);
	   to_do := Tcp_Tcb.Q.add (! to_do, Tcp_Tcb.Log_Event log)
	end

(*
	6.	function acknowledge
*)

   val probe = Tcp_Tcb.Probe_Window

   fun seq_last (Tcp_Tcb.Seg {seq, len, syn_flag, fin_flag, options, ...}) =
        let val correction = if syn_flag orelse fin_flag then one32
			     else zero32
	in (seq, Word32.+ (seq, Word32.+ (len, correction)))
	end

  in (* local *)

   fun acknowledge (state, tcb, ack, send_time) =
       let val Tcp_Tcb.Tcb {to_do, resend, ...} = tcb
	   val first = Tcp_Tcb.D.first (! resend)
	   val found_one = case first of NONE => false | _ => true
(* Restart the resend timer for this segment using the latest timeout
   value, minus the time since we sent this segment.  The "min"
   operations are to protect against bad values of "start" and against
   the timer having logically already timed out. *)
	   fun restart_timer start =
	        let val Tcp_Tcb.Tcb {timers, srto, ...} = tcb
		    val now = Tcp_Tcb.current_time ()
		    val gone = now - Word32.min (now, start)
		    val timeout = ! srto
		    val timer = timeout - Word32.min (timeout, gone)
		in (#start_resend timers) timer
		end
(* ack_loop takes acked segments off the resend queue.  If the resend
   queue has been emptied, clear the resend and window timers, and
   probe the window if there are segments waiting to be sent.

   The start time is taken to be either the time since sending the
   segment (which does not correspond to RFC 1323, p. 36, which
   says to use the elapsed time since the first segment in the
   retransmission queue) or the time in the timestamp option. *)
	   fun ack_loop (NONE, _) =
	        if found_one andalso Tcp_Tcb.D.empty (! resend) then
		 let val Tcp_Tcb.Tcb {timers, queued, ...} = tcb
		 in (#stop_resend timers) ();
		    (#stop_window timers) ();
		    if not (Tcp_Tcb.D.empty (! queued)) then
		     to_do := Tcp_Tcb.Q.add (! to_do, probe)
		    else ()
		 end
		else ()
	     | ack_loop (SOME (new_queue, (segment, start, resent)), acked) =
	        let val start_time = case send_time of SOME x => x | _ => start
		    val (seq, last_seq_plus_one) =
		          case segment of
			     Tcp_Tcb.Fast_Out {seq, len, ...} =>
			      (seq, Word32.+ (seq, len))
			   | Tcp_Tcb.Fast_Empty {seq, ...} => (seq, seq)
			   | Tcp_Tcb.Timestamp_Out {seq, len, ...} =>
			      (seq, Word32.+ (seq, len))
			   | Tcp_Tcb.Timestamp_Empty {seq, ...} => (seq, seq)
			   | Tcp_Tcb.Out_Seg {seg, data} => seq_last seg
		    (* val last_seq = Word32.+ (seq, len) *)
		    val delta = Word32.- (ack, last_seq_plus_one)
		in (* in-line b4less if b4less (ack, last_seq_plus_one) then *)
		   if Word32.> (delta, signed_min_b4) then
(* this segment is not acked yet. If others have been, restart the
   resend timer for this segment using the latest timeout value,
   minus the time since we sent this segment. Otherwise, do not
   restart the timer. *)
		    if acked then restart_timer start_time else ()
		   else
		    (if resent = Tcp_Tcb.Never_Resent andalso
		        delta = zero32 then
		      (* Karn's algorithm [1122 p. 95] *)
		      (recompute_srtt (start_time, tcb);
		       (* Jacobson's slow start algorithm [1122 p. 90] *)
		       recompute_cwnd tcb)
		     else ();
		     resend := new_queue;
		     ack_loop (Tcp_Tcb.D.first new_queue, true))
		end
       in ack_loop (first, false);
	  if found_one then
	   to_do := Tcp_Tcb.Q.add (! to_do, Tcp_Tcb.Complete_Send ack)
	  else ();
	  tcb
       end

  end (* local *)
 end (* struct *)
