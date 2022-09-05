(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edo Biagioni (esb@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

	i.	Abstract

	tcpsend.tst: simple conformance-to-RFC test for the
	TCP send operation.

---------------------------------------------------------------------

	ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor Test_Tcp_Send
	2.	internal function lower_allocate
	3.	internal function fill_segment
	4.	internal function build_segment
	5.	internal function build_out
	6.	internal function same_segment
	7.	internal function same_action
	8.	function check_same_set
	9.	function check_to_do
	10.	internal function check_in_queue
	11.	internal function check_retransmit
	12.	internal function check_send
	13.	internal function build_estab
	14.	function test_small
	15.	function test_urgent
	16.	function test_large
	17.	function test_beyond_window
	18.	function test_window_segment
	19.	function test_zero_window
	20.	function test_zero_packet
	21.	function run_tests
	22.	function run
	23.	structure Test_Tcp_Send

---------------------------------------------------------------------
	iii.	RCS Log

$Log: tcpsend.tst,v $
Revision 1.23  1995/03/12  17:52:02  esb
adapted to new trace.sig.

Revision 1.22  1995/03/10  03:47:37  esb
adapted to new vendor.sig.

Revision 1.21  1995/03/07  23:57:08  esb
updated tracing.

Revision 1.20  1995/02/21  13:05:02  esb
upgraded for SML/NJ 1.07.

Revision 1.19  1995/02/13  23:28:49  esb
adapted for 1.07.

Revision 1.18  1995/01/14  02:28:30  esb
./tcpstateadded tcp window-scale option.

Revision 1.17  1994/10/11  15:43:04  cline
Upgraded to work with SML/NJ 1.05b

Revision 1.16  1994/08/18  20:32:06  esb
adapted to new actions, support for status messages.

Revision 1.15  1994/08/17  16:28:26  esb
minor tuning and bug fixes.

Revision 1.14  1994/08/12  06:27:15  esb
converted to the new protocol signature.

Revision 1.13  1994/07/01  02:32:46  danwang
Moved control structures into Fox_Basis.

Revision 1.12  1994/06/16  16:42:04  danwang
Updated to use functorized Fox_Basis

Revision 1.11  1994/06/13  23:13:13  esb
moved timer implementation from tcpaction to tcptcb.

Revision 1.10  1994/05/10  08:08:18  esb
added clauses for Fast_Out and Fast_Push to same_segment.

Revision 1.9  94/03/11  04:59:39  esb
adapted to new send interface.

Revision 1.8  1994/03/07  16:56:02  esb
adjusted to turning on push bit at end of each burst.

Revision 1.7  94/02/21  00:05:31  esb
fixed a minor bug.

Revision 1.6  94/02/17  01:12:24  esb
interface changes.

Revision 1.5  94/02/14  14:26:44  esb
now ignores log messages.

Revision 1.4  94/01/30  21:00:19  esb
minor changes.

Revision 1.3  1994/01/28  01:14:37  esb
adapted to new interface.

Revision 1.2  1994/01/19  21:30:53  esb
adapted to new interface.

Revision 1.1  1994/01/09  03:22:47  esb
Initial revision

*)

(*
---------------------------------------------------------------------
	1.	functor Test_Tcp_Send
 *)

functor Test_Tcp_Send (structure B: FOX_BASIS
		       val debug_level: int ref option): TEST_STRUCTURE =
 struct
  structure Trace = Trace (structure V = B.V
			   val debug_level = debug_level
			   val module_name = "tcpsend.tst")
  val local_print = Trace.local_print
  val debug_print = Trace.debug_print

  structure Log = Tcp_Log (structure B = B)

  structure Tcb = Tcp_Tcb (structure B = B
			   structure Tcp_Log = Log)

  structure Retransmit = Tcp_Retransmit (structure Tcp_Tcb = Tcb
					 structure B = B)

  structure Send = Tcp_Send (structure Tcp_Tcb = Tcb
			     structure Retransmit = Retransmit
			     structure B = B
			     val debug_level = debug_level)

(*
---------------------------------------------------------------------
	2.	internal function lower_allocate
*)

  fun lower_allocate size =
       (B.Dyn_Array.new size,
	fn segment =>
	  local_print ("sending " ^ Tcb.out_string segment))

(*
---------------------------------------------------------------------
	3.	internal function fill_segment
*)

  fun fill_segment (segment, offset) =
       let fun fill (n, _) = FoxWord8.intToWord ((n + offset) mod 256)
       in B.Dyn_Array.app1 (segment, fill)
       end

(*
---------------------------------------------------------------------
	4.	internal function build_segment
*)

  fun build_segment (len, offset) =
       let fun fill n = FoxWord8.intToWord ((n + offset) mod 256)
       in B.Dyn_Array.init1 (len, fill)
       end

(*
---------------------------------------------------------------------
	5.	internal function build_out
*)

  fun build_out (seq, ack, len, wnd, push, urgent) =
       let val seg = Tcb.Seg {seq = FoxWord32.intToWord seq,
			      ack = FoxWord32.intToWord ack,
			      len = FoxWord32.intToWord len,
			      wnd = FoxWord32.intToWord wnd,
			      up = if urgent then FoxWord32.intToWord (len - 1)
				   else SW.n32 "0",
			      options = [],
			      syn_flag = false,
			      fin_flag = false,
			      reset_flag = false,
			      ack_flag = true,
			      push_flag = push,
			      urgent_flag = urgent}
	   val out_data = build_segment (len, seq)
	   val out = Tcb.Out_Seg {seg = seg, data = out_data}
       in out
       end

(*
---------------------------------------------------------------------
	6.	internal function same_segment
*)

  fun rebuild_segment (seq, ack, len, wnd, data, push) =
       let val new_seg = Tcb.Seg {seq = seq, ack = ack, len = len, wnd = wnd,
				  up = SW.n32 "0", options = [],
				  syn_flag = false,
				  fin_flag = false, ack_flag = true,
				  reset_flag = false, push_flag = push,
				  urgent_flag = false}
       in Tcb.Out_Seg {seg = new_seg, data = data}
       end

  fun same_segment (Tcb.Fast_Out {seq, ack, len, wnd, data}, segment) =
       same_segment (rebuild_segment (seq, ack, len, wnd, data, true),
		     segment)
    | same_segment (Tcb.Fast_Empty {seq, ack, wnd}, segment) =
       same_segment (rebuild_segment (seq, ack, SW.n32 "0", wnd,
				      B.Dyn_Array.new 0, false),
		     segment)
    | same_segment (segment, Tcb.Fast_Out {seq, ack, len, wnd, data}) =
       same_segment (segment,
		     rebuild_segment (seq, ack, len, wnd, data, true))
    | same_segment (segment, Tcb.Fast_Empty {seq, ack, wnd}) =
       same_segment (segment,
		     rebuild_segment (seq, ack, SW.n32 "0", wnd,
				      B.Dyn_Array.new 0, true))
    | same_segment (Tcb.Out_Seg {seg = seg1, data = data1},
		    Tcb.Out_Seg {seg = seg2, data = data2}) =
       let val Tcb.Seg {seq = seq1, ack = ack1, len = len1, wnd = wnd1,
			up = up1, options = options1, syn_flag = syn1,
			fin_flag = fin1, ack_flag = acf1, reset_flag = rst1,
			push_flag = psh1, urgent_flag = urg1} = seg1
	   val Tcb.Seg {seq = seq2, ack = ack2, len = len2, wnd = wnd2,
			up = up2, options = options2, syn_flag = syn2,
			fin_flag = fin2, ack_flag = acf2, reset_flag = rst2,
			push_flag = psh2, urgent_flag = urg2} = seg2
	   fun same_bytes (b1, off1, l1, b2, off2, l2) =
	        if off1 = l1 andalso off2 = l2 then true
		else if off1 >= l1 orelse off2 >= l2 then
		 (debug_print (fn _ => "same_segment failed, seeing " ^
			       B.V.Integer.makestring off1 ^ "/" ^
			       B.V.Integer.makestring l1 ^ ", expected " ^
			       B.V.Integer.makestring off2 ^ "/" ^
			       B.V.Integer.makestring l2);
		  false)
		else if ByteArray.sub (b1, off1) <>
		        ByteArray.sub (b2, off2) then
		 (debug_print (fn _ => "same_segment failed, byte [" ^
			       B.V.Integer.makestring off1 ^ "] =" ^
			       B.V.Integer.makestring (ByteArray.sub
						       (b1, off1)) ^
			       ", expected [" ^
			       B.V.Integer.makestring off2 ^ "] = " ^
			       B.V.Integer.makestring (ByteArray.sub
						       (b2, off2)) ^
			       ", l1 =" ^
			       B.V.Integer.makestring l1 ^ ", l2 = " ^
			       B.V.Integer.makestring l2);
		  false)
	        else same_bytes (b1, off1 + 1, l1, b2, off2 + 1, l2)
	   val l1 = B.Dyn_Array.size data1
	   val l2 = B.Dyn_Array.size data2
	   val b1 = B.Dyn_Array.read data1
	   val b2 = B.Dyn_Array.read data2
	   val t1 = l1 = l2
	   val t2 = same_bytes (b1, 0, l1, b2, 0, l2)
	   val t3 = seq1 = seq2
	   val t4 = not acf1 orelse ack1 = ack2
	   val t5 = len1 = len2
	   val t6 = wnd1 = wnd2
	   val t7 = not urg1 orelse up1 = up2
	   val t8 = syn1 = syn2 andalso fin1 = fin2 andalso
		    acf1 = acf2 andalso rst1 = rst2 andalso
		    urg1 = urg2 andalso psh1 = psh2
	   val result = t1 andalso t2 andalso t3 andalso t4 andalso
	                t5 andalso t6 andalso t7 andalso t8
	   val ack_string = if not acf1 then ""
			    else (FoxMakestring.word32 ack1 ^ "/" ^
				  FoxMakestring.word32 ack2 ^ ", ")
	   val urg_string = if not urg1 then "\n"
			    else (FoxMakestring.word32 up1 ^ "/" ^
				  FoxMakestring.word32 up2 ^ ",\n")
	   val bytes_string =
	        if t2 orelse 1 >= l1 orelse 1 >= l2 then ""
		else (", bytes are " ^
		      FoxMakestring.word8 (FoxWord8.sub (b1, 0)) ^ "." ^
		      FoxMakestring.word8 (FoxWord8.sub (b1, 1)) ^ ", " ^
		      FoxMakestring.word8 (FoxWord8.sub (b2, 0)) ^ "." ^
		      FoxMakestring.word8 (FoxWord8.sub (b2, 1)))
	   val res_string =
	    "(" ^ FoxMakestring.bool t1 ^ " & " ^
	          FoxMakestring.bool t2 ^ " & " ^
	          FoxMakestring.bool t3 ^ " & " ^
		  FoxMakestring.bool t4 ^ " & " ^
		  FoxMakestring.bool t5 ^ " & " ^
		  FoxMakestring.bool t6 ^ " & " ^
		  FoxMakestring.bool t7 ^ " & " ^
		  FoxMakestring.bool t8 ^ ")\n"
	   val num_true = (if t1 then 1 else 0) + (if t2 then 1 else 0) +
	                  (if t3 then 1 else 0) + (if t4 then 1 else 0) +
			  (if t5 then 1 else 0) + (if t6 then 1 else 0) +
			  (if t7 then 1 else 0) + (if t8 then 1 else 0)
       in if result then true
	  else
	   (if num_true > 5 orelse
	       (Trace.debug_on () andalso num_true > 4) then
	     local_print
	        ("size1 = " ^ makestring (B.Dyn_Array.size data1) ^ ", " ^
		 "size2 = " ^ makestring (B.Dyn_Array.size data2) ^
		 bytes_string ^ "\n" ^
		 "seg1 = " ^ Tcb.segment_string seg1 ^ ", " ^
		 "seg2 = " ^ Tcb.segment_string seg2 ^ "\n" ^
		 FoxMakestring.word32 seq1 ^ "/" ^
		 FoxMakestring.word32 seq2 ^ ", " ^
		 ack_string ^ makestring len1 ^ "/" ^
		 makestring len2 ^ ", " ^
	         FoxMakestring.word32 wnd1 ^ "/" ^
		 FoxMakestring.word32 wnd2 ^ ", " ^
		 urg_string ^
		  FoxMakestring.bool syn1 ^ "/" ^
		  FoxMakestring.bool syn2 ^ ", " ^
		  FoxMakestring.bool fin1 ^ "/" ^
		  FoxMakestring.bool fin2 ^ ", " ^
		  FoxMakestring.bool acf1 ^ "/" ^ 
		  FoxMakestring.bool acf2 ^ ", " ^
		  FoxMakestring.bool rst1 ^ "/" ^
		  FoxMakestring.bool rst2 ^ ", " ^
		  FoxMakestring.bool urg1 ^ "/" ^
		  FoxMakestring.bool urg2 ^ ", " ^
		  FoxMakestring.bool psh1 ^ "/" ^
		  FoxMakestring.bool psh2 ^ "\n" ^
		  res_string)
	    else ();
	    false)
       end (* let *)

(*
---------------------------------------------------------------------
	7.	internal function same_action
*)

  fun same_action (Tcb.User_Error s1, Tcb.User_Error s2) = s1 = s2
    | same_action (Tcb.Send_Segment (s1, _), Tcb.Send_Segment (s2, _)) =
       same_segment (s1, s2)
    | same_action (Tcb.User_Data packet1, Tcb.User_Data packet2) =
       B.Dyn_Array.size packet1 = B.Dyn_Array.size packet2
    | same_action (Tcb.Urgent_Data packet1, Tcb.Urgent_Data packet2) =
       B.Dyn_Array.size packet1 = B.Dyn_Array.size packet2
    | same_action (Tcb.Close_After_Sends, Tcb.Close_After_Sends) = true
    | same_action (Tcb.Complete_Open x, Tcb.Complete_Open y) = x = y
    | same_action (Tcb.Complete_Close x, Tcb.Complete_Close y) = x = y
    | same_action (Tcb.Complete_Send x, Tcb.Complete_Send y) = x = y
    | same_action (Tcb.Probe_Window, Tcb.Probe_Window) = true
    | same_action (Tcb.Log_Event _, Tcb.Log_Event _) = true
    | same_action (Tcb.Peer_Close, Tcb.Peer_Close) = true
    | same_action (Tcb.Peer_Reset, Tcb.Peer_Reset) = true
    | same_action (a, b) = false

(*
---------------------------------------------------------------------
	8.	function check_same_set
*)

  fun check_same_set (same, [], []) = true
    | check_same_set (same, [], _) = false
    | check_same_set (same, _, []) = false
    | check_same_set (same, actual, expected) =
       let fun is_same e1 e2 = same (e1, e2)
	   fun member (element, l) = exists (is_same element) l
	   fun also_member l (element, previous) =
	        previous andalso member (element, l)
	   fun subset (l1, l2) = B.V.List.fold (also_member l2) l1 true
       in subset (actual, expected) andalso subset (expected, actual)
       end

(*
---------------------------------------------------------------------
	9.	function check_to_do
*)

  fun check_to_do (to_do, expected) =
       let fun build (queue, res) =
	        case Tcb.Q.next queue of
		   NONE => rev res
		 | SOME (new_queue, Tcb.Log_Event _) => build (new_queue, res)
		 | SOME (new_queue, next) => build (new_queue, next :: res)
	   val to_do_list = build (to_do, [])
	   val res = check_same_set (same_action, to_do_list, expected)
	   fun print_action (action, "") = Tcb.action_string action
	     | print_action (action, s) = (Tcb.action_string action ^ ", " ^ s)
       in if res then true
	  else
	   (local_print ("check_to_do failed: to_do list has length " ^
			 B.V.Integer.makestring (B.V.List.length to_do_list) ^
			 ", expecting " ^

			 B.V.Integer.makestring (B.V.List.length expected) ^
			 ", lists are:\n[" ^
			 (B.V.List.fold print_action to_do_list "") ^
			 "] and [" ^
			 (B.V.List.fold print_action expected "") ^ "]");
	    false)
       end

(*
---------------------------------------------------------------------
	10.	internal function check_in_queue
*)

  fun check_in_queue (queue, segment) =
       case (Tcb.D.first queue, segment) of
	  (NONE, NONE) => true
	| (NONE, _) =>
	   (local_print ("check_in_queue failed, segment is not NONE");
	    false)
	| (SOME (new_queue, (seg, b4size, urgent, _)),
	   SOME (expected, expected_urgent)) =>
	   let fun same_bytes (b1, off1, l1, b2, off2, l2) =
	            if off1 = l1 andalso off2 = l2 then true
		    else if off1 >= l1 orelse off2 >= l2 then
		     (debug_print (fn _ => "check_in_queue failed, seeing " ^
				   B.V.Integer.makestring off1 ^ "/" ^
				   B.V.Integer.makestring l1 ^ ", expected " ^
				   B.V.Integer.makestring off2 ^ "/" ^
				   B.V.Integer.makestring l2);
		     false)
		    else if ByteArray.sub (b1, off1) <>
		            ByteArray.sub (b2, off2) then
		     (debug_print (fn _ => "check_in_queue failed, byte [" ^
				   B.V.Integer.makestring off1 ^ "] = " ^
				   B.V.Integer.makestring (ByteArray.sub
							   (b1, off1)) ^
				   ", expected [" ^
				   B.V.Integer.makestring off2 ^ "] = " ^
				   B.V.Integer.makestring (ByteArray.sub
							   (b2, off2)) ^
				   ", l1 =" ^
				   B.V.Integer.makestring l1 ^ ", l2 = " ^
				   B.V.Integer.makestring l2);
		      false)
	            else same_bytes (b1, off1 + 1, l1, b2, off2 + 1, l2)
	       val size1 = B.Dyn_Array.size seg
	       val size2 = B.Dyn_Array.size expected
	       val b1 = B.Dyn_Array.read seg
	       val b2 = B.Dyn_Array.read expected
	   in if size1 = size2 andalso
	         size1 = FoxWord32.wordToInt b4size andalso
	         urgent = expected_urgent andalso
	         same_bytes (b1, 0, size1, b2, 0, size2) then true
	      else
	       (local_print ("check_in_queue failed, segment size is " ^
			     B.V.Integer.makestring size1 ^
			     (if urgent then " (urgent)" else "") ^
			       ", expecting " ^
			       B.V.Integer.makestring size2 ^
			       (if expected_urgent then " (urgent)" else ""));
		false)
	   end
	| _ =>
	   (local_print ("check_in_queue failed, segment is NONE");
	    false)

(*
---------------------------------------------------------------------
	11.	internal function check_retransmit
*)

  fun check_retransmit (tcb as (Tcb.Tcb {resend, ...}), []) =
       (case Tcb.D.first (! resend) of
	   NONE => true
	 | SOME (new_resend, first) =>
	    (local_print ("check_retransmit failed, something in queue");
	     false))
    | check_retransmit (tcb as (Tcb.Tcb {resend, ...}), expected :: rest) =
       let fun same_send (seg1, seg2) =
	        if same_segment (seg1, seg2) then true
		else
		 (local_print ("segments not equal, retransmitting " ^
			       Tcb.out_string seg1 ^ ", expecting " ^
			       Tcb.out_string seg2);
		  false)
       in case Tcb.D.first (! resend) of
	     NONE =>
	      (local_print ("check_retransmit failed, empty queue");
	       false)
	   | SOME (new_resend, (first, time, resent)) =>
	      (resend := new_resend;
	       if same_send (first, expected) then
		check_retransmit (tcb, rest)
	       else
		(local_print ("check_retransmit failed");
		 false))
       end

(*
---------------------------------------------------------------------
	12.	internal function check_send
*)

  fun check_send (expected_sends, expected_queued, expected_actions,
		  expected_snd_nxt, tcb, queued, to_do, snd_nxt) =
       check_to_do (! to_do, expected_actions) andalso
       check_retransmit (tcb, expected_sends) andalso
       check_in_queue (! queued, expected_queued) andalso
       (if ! snd_nxt = (FoxWord32.intToWord expected_snd_nxt) then true
	else (local_print ("snd.nxt is " ^
			   FoxMakestring.word32 (! snd_nxt) ^ ", expecting " ^
			   FoxMakestring.word32 (FoxWord32.intToWord
						 expected_snd_nxt));
	      false))

(*
---------------------------------------------------------------------
	13.	internal function build_estab
*)

  fun build_estab (snd_una, snd_nxt, snd_wnd, rcv_nxt, mss, resend_size) =
       let fun build_resend (0, tcb) = ()
	     | build_resend (n, tcb) =
	        let val packet = build_out (1, 2, 3 + n, 4, false, false)
		in Retransmit.retransmit (tcb, packet, 1000);
		   build_resend (n - 1, tcb)
		end
	   fun act _ = debug_print (fn _ => "timer expired")
	   val to_do_ref = ref (Tcb.Q.new ())
	   val large_int = 30000
	   val tcb = Tcb.Tcb {iss = SW.n32 "9",
			      snd_una = ref (FoxWord32.intToWord snd_una),
			      snd_nxt = ref (FoxWord32.intToWord snd_nxt),
			      snd_wnd = ref (FoxWord32.intToWord snd_wnd),
			      max_snd_wnd = ref (FoxWord32.intToWord snd_wnd),
			      snd_wl1 = ref (SW.n32 "12"),
			      snd_wl2 = ref (SW.n32 "13"),
			      snd_wnd_scale = ref 0,
			      irs = ref (SW.n32 "14"),
			      rcv_nxt = ref (FoxWord32.intToWord rcv_nxt),
			      rcv_wnd = ref (SW.n32 "1024"),
			      rcv_sws = ref (SW.n32 "0"),
			      rcv_wnd_scale = ref 0,
			      wanted_wnd_scale = FoxWord8.intToWord 0,
			      send_immediately = ref false,
			      cwnd = ref large_int,
			      ssthresh = ref large_int,
			      unacked_segs = ref 0,
			      srtt = ref 16,
			      srtd = ref 17,
			      srto = ref 18,
			      wto = ref 19,
			      mss = ref (FoxWord16.intToWord mss),
			      resend = ref (Tcb.D.new ()),
			      queued = ref (Tcb.D.new ()),
			      out_of_order = ref (Tcb.Q.new ()),
			      timers = Tcb.empty_timer_set act,
			      to_do = to_do_ref}
       in build_resend (resend_size, tcb);
	(* build_resend may put commands int the to_do list,
	   so we get rid of them *)
	  to_do_ref := Tcb.Q.new ();
	  Tcb.Estab tcb
       end

(*
---------------------------------------------------------------------
	14.	function test_small

	Send a small packet which fits in the window and
	can be carried in a single segment.
*)

  fun test_small () =
       let val s0 = build_estab (10100, 10100, 4096, 33333, 1460, 0)
	   val (s1, segment, send) =
	          case Send.allocate (s0, 50, false, lower_allocate) of
		     (state, NONE) =>
		      (state, build_segment (50, 10100), fn s => s)
		   | (state, SOME (segment, send)) => (state, segment, send)
	   val _ = fill_segment (segment, 10100)
	   val s2 = send s1
	   val out = build_out (10100, 33333, 50, 1024, true, false)
	   val actions = [Tcb.Send_Segment (out, NONE)]
       in case s2 of
	     Tcb.Estab (tcb as (Tcb.Tcb {to_do, queued, snd_nxt, ...})) =>
	      check_send ([out], NONE, actions, 10150, tcb, queued,
			  to_do, snd_nxt)
	   | _ =>
	      (local_print ("error in test_small, state is " ^
			    Tcb.state_string s1);
	       false)
       end

(*
---------------------------------------------------------------------
	15.	function test_urgent

	Send a small urgent packet which fits in the window and
	can be carried in a single segment.
*)

  fun test_urgent () =
       let val s0 = build_estab (10100, 10100, 4096, 33333, 1460, 0)
	   val (s1, segment, send) =
	          case Send.allocate (s0, 50, true, lower_allocate) of
		     (state, NONE) =>
		      (state, build_segment (50, 10100), fn s => s)
		   | (state, SOME (segment, send)) => (state, segment, send)
	   val _ = fill_segment (segment, 10100)
	   val s2 = send s1
	   val out = build_out (10100, 33333, 50, 1024, true, true)
	   val actions = [Tcb.Send_Segment (out, NONE)]
       in case s2 of
	     Tcb.Estab (tcb as (Tcb.Tcb {to_do, queued, snd_nxt, ...})) =>
	      check_send ([out], NONE, actions, 10150, tcb, queued,
			  to_do, snd_nxt)
	   | _ =>
	      (local_print ("error in test_small, state is " ^
			    Tcb.state_string s1);
	       false)
       end

(*
---------------------------------------------------------------------
	16.	function test_large

	Send a packet which is too large to fit in a single segment,
	but still fits in the window.
*)

  fun test_large () =
       let val s0 = build_estab (10000, 10100, 3996, 33333, 536, 1)
	   val (s1, segment, send) =
	          case Send.allocate (s0, 1000, false, lower_allocate) of
		     (state, NONE) =>
		      (state, build_segment (1000, 10100), fn s => s)
		   | (state, SOME (segment, send)) => (state, segment, send)
	   val _ = fill_segment (segment, 10100)
	   val s2 = send s1
	   val out1 = build_out (10100, 33333, 536, 1024, true, false)
	   val out2 = build_out (10636, 33333, 464, 1024, true, false)
	   val actions = [Tcb.Send_Segment (out1, NONE),
			  Tcb.Send_Segment (out2, NONE)]
       in case s2 of
	     Tcb.Estab (tcb as (Tcb.Tcb {to_do, queued, snd_nxt,
					 resend, ...})) =>
	      (case Tcb.D.first (! resend) of
		  NONE => ()
		| SOME (new_queue, _) => resend := new_queue;
		   (* get rid of the retransmit segment that
		      was placed here by build_estab *)
	       check_send ([out1, out2], NONE, actions, 11100,
			   tcb, queued, to_do, snd_nxt))
	   | _ =>
	      (local_print ("error in test_large, state is " ^
			    Tcb.state_string s1);
	       false)
       end

(*
---------------------------------------------------------------------
	17.	function test_beyond_window

	Send a packet which is larger than the send window
	(but smaller than a segment).
*)

  fun test_beyond_window () =
       let val s0 = build_estab (91000, 91000, 100, 33333, 536, 1)
	   val (s1, segment, send) =
	          case Send.allocate (s0, 200, false, lower_allocate) of
		     (state, NONE) =>
		      (state, build_segment (200, 10100), fn s => s)
		   | (state, SOME (segment, send)) => (state, segment, send)
	   val _ = fill_segment (segment, 91000)
	   val s2 = send s1
	   val out = build_out (91000, 33333, 100, 1024, true, false)
	   val q = (build_segment (100, 91100), false)
	   val actions = [Tcb.Send_Segment (out, NONE)]
       in case s2 of
	     Tcb.Estab (tcb as (Tcb.Tcb {to_do, queued, resend,
					 snd_nxt, ...})) =>
	      (case Tcb.D.first (! resend) of
		  NONE => ()
		| SOME (new_queue, _) => resend := new_queue;
		   (* get rid of the retransmit segment that
		      was placed here by build_estab *)
	       check_send ([out], SOME q, actions, 91100,
			   tcb, queued, to_do, snd_nxt))
	   | _ =>
	      (local_print ("error in test_beyond_window, state is " ^
			    Tcb.state_string s1);
	       false)
       end

(*
---------------------------------------------------------------------
	18.	function test_window_segment

	Send a packet which is larger than the send window and larger
	than a segment; the segment is smaller than the window.
*)

  fun test_window_segment () =
       let val s0 = build_estab (10000, 10050, 1000, 33333, 536, 0)
	   val (s1, segment, send) =
	          case Send.allocate (s0, 1000, false, lower_allocate) of
		     (state, NONE) =>
		      (state, build_segment (1000, 10100), fn s => s)
		   | (state, SOME (segment, send)) => (state, segment, send)
	   val _ = fill_segment (segment, 10050)
	   val s2 = send s1
	   val out1 = build_out (10050, 33333, 536, 1024, true, false)
	   val q = (build_segment (464, 10586), false)
	   val actions = [Tcb.Send_Segment (out1, NONE)]
       in case s2 of
	     Tcb.Estab (tcb as (Tcb.Tcb {to_do, queued, snd_nxt, ...})) =>
	      check_send ([out1], SOME q, actions, 10586,
			  tcb, queued, to_do, snd_nxt)
	   | _ =>
	      (local_print ("error in test_window_segment, state is " ^
			    Tcb.state_string s1);
	       false)
       end

(*
---------------------------------------------------------------------
	19.	function test_zero_window
*)

  fun test_zero_window () =
       let val s0 = build_estab (10100, 10100, 0, 33333, 536, 0)
	   val (s1, segment, send) =
	          case Send.allocate (s0, 10, false, lower_allocate) of
		     (state, NONE) =>
		      (state, build_segment (10, 10100), fn s => s)
		   | (state, SOME (segment, send)) => (state, segment, send)
	   val _ = fill_segment (segment, 10100)
	   val s2 = send s1
	   val out = build_out (10100, 33333, 1, 1024, true, false)
	   val q = (build_segment (9, 10101), false)
	   val actions = [Tcb.Send_Segment (out, NONE)]
       in case s2 of
	     Tcb.Estab (tcb as (Tcb.Tcb {to_do, queued, snd_nxt, ...})) =>
	      check_send ([out], SOME q, actions, 10101,
			  tcb, queued, to_do, snd_nxt)
	   | _ =>
	      (local_print ("error in test_zero_window, state is " ^
			    Tcb.state_string s1);
	       false)
       end

(*
---------------------------------------------------------------------
	20.	function test_zero_packet
*)

  fun test_zero_packet () =
       let val s0 = build_estab (10000, 10000, 4096, 33333, 536, 0)
	   val (s1, segment, send) =
	          case Send.allocate (s0, 0, false, lower_allocate) of
		     (state, NONE) =>
		      (state, build_segment (0, 10000), fn s => s)
		   | (state, SOME (segment, send)) => (state, segment, send)
	   val _ = fill_segment (segment, 10000)
	   val s2 = send s1
       in case s2 of
	     Tcb.Estab (tcb as (Tcb.Tcb {to_do, queued, snd_nxt, ...})) =>
	      check_send ([], NONE, [], 10000,
			  tcb, queued, to_do, snd_nxt)
	   | _ =>
	      (local_print ("error in test_zero_packet, state is " ^
			    Tcb.state_string s1);
	       false)
       end

(*
---------------------------------------------------------------------
	21.	function run_tests
*)

  fun run_tests () =
       (B.Test.test ("small", test_small);
        B.Test.test ("urgent", test_urgent);
        B.Test.test ("large", test_large);
        B.Test.test ("beyond window", test_beyond_window);
        B.Test.test ("window segment", test_window_segment);
        B.Test.test ("zero window", test_zero_window);
        B.Test.test ("zero packet", test_zero_packet);
        ())

(*
---------------------------------------------------------------------
	22.	function run
*)

  fun run () =
       B.Test.tests ("TcpSend", 7, run_tests)

  val _ = if ! B.Debug.do_tests then run () else ()

 end (* struct *)

(*
---------------------------------------------------------------------
		23.	structure Test_Tcp_Send
*)

structure Test_Tcp_Send = Test_Tcp_Send (structure B = Fox_Basis
					 val debug_level = NONE)
