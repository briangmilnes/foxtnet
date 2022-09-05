(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edo Biagioni (esb@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

	i.	Abstract

	tcpreceive.tst: simple conformance-to-RFC test for the
	TCP data receive operation.

---------------------------------------------------------------------

	ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor Test_Tcp_Receive
	2.	internal function build_segment
	3.	internal function build_retransmit
	4.	internal function build_in
	5.	internal function same_segment
	6.	internal function same_out
	7.	internal function same_action
	8.	function check_same_set
	9.	function check_to_do
	10.	internal function check_retransmit
	11.	internal function check_out_of_order
	12.	internal function check_receive
	13.	internal function build_estab
	14.	function test_small
	15.	function test_urgent
	16.	function test_part_urgent
	17.	function test_beyond_window
	18.	function test_large_segment
	19.	function test_zero_window
	20.	function test_probe_window
	21.	function test_zero_packet
	22.	function test_early_packet
	23.	function test_late_packet
	24.	function run_tests
	25.	function run
	26.	structure Test_Tcp_Receive

---------------------------------------------------------------------
	iii.	RCS Log

$Log: tcpreceive.tst,v $
Revision 1.22  1995/03/12  17:52:02  esb
adapted to new trace.sig.

Revision 1.21  1995/03/07  23:57:08  esb
updated tracing.

Revision 1.20  1995/02/21  13:05:02  esb
upgraded for SML/NJ 1.07.

Revision 1.19  1995/01/14  02:28:30  esb
./tcpstateadded tcp window-scale option.

Revision 1.18  1994/10/11  15:43:04  cline
Upgraded to work with SML/NJ 1.05b

Revision 1.17  1994/08/18  20:32:06  esb
adapted to new actions, support for status messages.

Revision 1.16  1994/08/17  16:28:26  esb
minor tuning and bug fixes.

Revision 1.15  1994/08/12  06:27:15  esb
converted to the new protocol signature.

Revision 1.14  1994/07/11  17:55:15  esb
Adapted to the new Fast_In message.

Revision 1.13  1994/07/01  02:32:39  danwang
Moved control structures into Fox_Basis.

Revision 1.12  1994/06/16  16:42:04  danwang
Updated to use functorized Fox_Basis

Revision 1.11  1994/06/13  23:13:13  esb
moved timer implementation from tcpaction to tcptcb.

Revision 1.10  1994/05/10  08:07:37  esb
adapted to new acking strategy in tcpreceive (no acks until window change).

Revision 1.9  94/04/06  23:18:11  esb
adapted to new receive_packet interface.

Revision 1.8  94/03/09  21:59:10  esb
added processing of acks from unacceptable segments.

Revision 1.7  1994/03/07  16:55:42  esb
adjusted to not sending acks immediately on receipt of packet

Revision 1.6  94/02/21  00:04:29  esb
minor changes to reflect changed TCP interfaces.

Revision 1.5  94/02/14  14:26:09  esb
now ignores log messages.

Revision 1.4  94/01/30  20:57:25  esb
added complete-send actions where needed.

Revision 1.3  1994/01/28  01:28:46  esb
adjusted to new high-performance algorithms.

Revision 1.2  1994/01/19  21:30:07  esb
updated to new interface.

Revision 1.1  1994/01/09  03:22:47  esb
Initial revision

*)

(*
---------------------------------------------------------------------
	1.	functor Test_Tcp_Receive
 *)

functor Test_Tcp_Receive (structure B: FOX_BASIS
			  val debug_level: int ref option): TEST_STRUCTURE =
 struct
  structure Trace = Trace (structure V = B.V
			   val debug_level = debug_level
			   val module_name = "tcpreceive.tst")
  val local_print = Trace.local_print
  val debug_print = Trace.debug_print

  structure Log = Tcp_Log (structure B = B)

  structure Tcb = Tcp_Tcb (structure B = B
			   structure Tcp_Log = Log)

  structure Retransmit = Tcp_Retransmit (structure Tcp_Tcb = Tcb
					 structure B = B)

  val wait_ms = 2222

  structure Receive = Tcp_Receive (structure Tcp_Tcb = Tcb
				   structure Retransmit = Retransmit
				   val wait_ms = wait_ms
				   val ack_timeout_ms = 100
				   structure B = B
				   val debug_level = debug_level)

(*
---------------------------------------------------------------------
	2.	internal function build_segment
*)

  fun build_segment (in_len, offset) =
       let val len = max (in_len, 1)
	   fun fill n = FoxWord8.intToWord ((n + offset) mod 256)
       in B.Dyn_Array.init1 (len, fill)
       end

(*
---------------------------------------------------------------------
	3.	internal function build_retransmit

	Once we have built the retransmit, we clear the to_do list.
*)

  fun build_retransmit (Tcb.Tcb {to_do, ...}, []) = to_do := Tcb.Q.new ()
    | build_retransmit (tcb, (offset, len) :: rest) =
       let fun fill n = FoxWord8.intToWord ((n + offset) mod 256)
	   val data = B.Dyn_Array.init1 (len, fill)
	   val seg = Tcb.Seg {seq = FoxWord32.intToWord offset,
			      ack = SW.n32 "0", len = FoxWord32.intToWord len,
			      wnd = SW.n32 "0", up = SW.n32 "0",
			      options = [],
			      syn_flag = false, fin_flag = false,
			      reset_flag = false, ack_flag = true,
			      push_flag = false, urgent_flag = false}
	   val out = Tcb.Out_Seg {seg = seg, data = data}
       in Retransmit.retransmit (tcb, out, 10000);
	  build_retransmit (tcb, rest)
       end

(*
---------------------------------------------------------------------
	4.	internal function build_in
*)

  fun build_in (seq, ack, len, wnd, push, urgent) =
       let val seg = Tcb.Seg {seq = FoxWord32.intToWord seq,
			      ack = FoxWord32.intToWord ack,
			      len = FoxWord32.intToWord len,
			      wnd = FoxWord32.intToWord wnd,
			      up = (case urgent of
			               NONE => SW.n32 "0"
				     | SOME x => FoxWord32.intToWord x),
			      options = [],
			      syn_flag = false,
			      fin_flag = false,
			      reset_flag = false,
			      ack_flag = true,
			      push_flag = push,
			      urgent_flag = urgent <> NONE}
	   val in_data = build_segment (len, seq)
	   val res = Tcb.In_Seg {seg = seg, data = in_data}
       in res
       end

(*
---------------------------------------------------------------------
	5.	internal function same_segment
*)

  fun same_segment (data1, data2) =
       let fun same_bytes (b1, off1, l1, b2, off2, l2) =
	        if off1 = l1 andalso off2 = l2 then true
	        else if off1 >= l1 orelse off2 >= l2 then false
	        else if ByteArray.sub (b1, off1) <>
		        ByteArray.sub (b2, off2) then false
	        else same_bytes (b1, off1 + 1, l1, b2, off2 + 1, l2)
	   val s1 = B.Dyn_Array.size data1
	   val s2 = B.Dyn_Array.size data2
	   val b1 = B.Dyn_Array.read data1
	   val b2 = B.Dyn_Array.read data2
	   val t1 = s1 = s2
	   val t2 = same_bytes (b1, 0, s1, b2, 0, s2)
	   val result = t1 andalso t2
	   val bytes_string =
	        if t2 then ""
		else (", bytes are " ^
		      FoxMakestring.word8 (FoxWord8.sub (b1, 0)) ^ "." ^
		      FoxMakestring.word8 (FoxWord8.sub (b1, 1)) ^ ", " ^
		      FoxMakestring.word8 (FoxWord8.sub (b2, 0)) ^ "." ^
		      FoxMakestring.word8 (FoxWord8.sub (b2, 1)))
	   val res_string =
	    "(" ^ FoxMakestring.bool t1 ^ " & " ^ FoxMakestring.bool t2 ^ ")"
	   val num_true = (if t1 then 1 else 0) + (if t2 then 1 else 0)
       in if result then true
	  else
	   (if num_true > 0 orelse
	       (Trace.debug_on () andalso num_true >= 0) then
	     local_print
	        ("size1 = " ^ makestring s1 ^ ", " ^
		 "size2 = " ^ makestring s2 ^ bytes_string ^ "\n" ^
		 res_string)
	    else ();
	    false)
       end

(*
---------------------------------------------------------------------
	6.	internal function same_out
*)

  fun rebuild_out (seq, ack, len, wnd, data, push) =
       let val new_seg = Tcb.Seg {seq = seq, ack = ack, len = len, wnd = wnd,
				  up = SW.n32 "0", options = [],
				  syn_flag = false,
				  fin_flag = false, ack_flag = true,
				  reset_flag = false, push_flag = push,
				  urgent_flag = false}
       in Tcb.Out_Seg {seg = new_seg, data = data}
       end

  fun same_out (Tcb.Fast_Out {seq, ack, len, wnd, data}, segment) =
       same_out (rebuild_out (seq, ack, len, wnd, data, true), segment)
    | same_out (Tcb.Fast_Empty {seq, ack, wnd}, segment) =
       same_out (rebuild_out (seq, ack, SW.n32 "0", wnd,
			      B.Dyn_Array.new 0, false), segment)
    | same_out (segment, Tcb.Fast_Out {seq, ack, len, wnd, data}) =
       same_out (segment, rebuild_out (seq, ack, len, wnd, data, true))
    | same_out (segment, Tcb.Fast_Empty {seq, ack, wnd}) =
       same_out (segment, rebuild_out (seq, ack, SW.n32 "0", wnd,
				       B.Dyn_Array.new 0, false))
    | same_out (Tcb.Out_Seg {seg = seg1, data = data1},
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
	        else if off1 >= l1 orelse off2 >= l2 then false
	        else if ByteArray.sub (b1, off1) <>
		        ByteArray.sub (b2, off2) then false
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
	        if t2 then ""
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
	   (if num_true > 1 orelse
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
       same_out (s1, s2)
    | same_action (Tcb.Process_Data (Tcb.Fast_In {seq, ack, len, wnd, data}),
		   x) =
       let val seg = Tcb.Seg {seq = seq, ack = ack, len = len, wnd = wnd,
			      up = SW.n32 "0", options = [],
			      syn_flag = false, fin_flag = false,
			      ack_flag = true, push_flag = true,
			      reset_flag = false, urgent_flag = false}
	   val input = Tcb.In_Seg {seg = seg, data = data}
       in same_action (Tcb.Process_Data input, x)
       end
    | same_action (x, y as (Tcb.Process_Data (Tcb.Fast_In {seq, ...}))) =
       same_action (y, x)
    | same_action (Tcb.Process_Data (Tcb.In_Seg {seg = s1, data = d1}),
		   Tcb.Process_Data (Tcb.In_Seg {seg = s2, data = d2})) =
       same_segment (d1, d2)
    | same_action (Tcb.User_Data packet1, Tcb.User_Data packet2) =
       same_segment (packet1, packet2)
    | same_action (Tcb.Urgent_Data packet1, Tcb.Urgent_Data packet2) =
       same_segment (packet1, packet2)
    | same_action (Tcb.Close_After_Sends, Tcb.Close_After_Sends) = true
    | same_action (Tcb.Complete_Open x, Tcb.Complete_Open y) = x = y
    | same_action (Tcb.Complete_Close x, Tcb.Complete_Close y) = x = y
    | same_action (Tcb.Complete_Send x, Tcb.Complete_Send y) = x = y
    | same_action (Tcb.Probe_Window, Tcb.Probe_Window) = true
    | same_action (Tcb.Delete_Tcb, Tcb.Delete_Tcb) = true
    | same_action (Tcb.Log_Event _, Tcb.Log_Event _) = true
    | same_action (Tcb.Peer_Close, Tcb.Peer_Close) = true
    | same_action (Tcb.Peer_Reset, Tcb.Peer_Reset) = true
    | same_action (a, b) = false

(*
---------------------------------------------------------------------
	8.	function check_same_set
*)

  fun check_same_set (same, [], []) = true
    | check_same_set (same, actual, expected) =
       let fun member (element, l) = exists (fn e => same (e, element)) l
	   fun subset (l1, l2) =
	        B.V.List.fold (fn (e, previous) =>
			        previous andalso member (e, l2)) l1 true
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
	10.	internal function check_retransmit
*)

  fun check_retransmit (tcb as Tcb.Tcb {resend, ...}, []) =
       (case Tcb.D.first (! resend) of
	   NONE => true
	 | SOME (new_resend, out) =>
	    (local_print ("check_retransmit failed, something in queue");
	     false))
    | check_retransmit (tcb as Tcb.Tcb {resend, ...},
			(expected_offset, expected_len) :: rest) =
       (case Tcb.D.first (! resend) of
	   NONE =>
	    (local_print ("check_retransmit failed, empty queue");
	     false)
	 | SOME (new_resend, (out, time, resent)) =>
	    (resend := new_resend;
	     let val (seq, len) =
	           case out of
		      Tcb.Out_Seg {seg = Tcb.Seg {seq, len, ...}, ...} =>
		       (seq, len)
		    | Tcb.Fast_Out {seq, len, ...} => (seq, len)
		    | Tcb.Fast_Empty {seq, ...} => (seq, SW.n32 "0")
	     in if expected_offset = FoxWord32.wordToInt seq andalso 
		   expected_len = FoxWord32.wordToInt len then
		 check_retransmit (tcb, rest)
		else
		 (local_print ("check_retransmit failed, offset = " ^
			       B.V.Integer.makestring (FoxWord32.wordToInt
						       seq) ^
			       ", expecting " ^
			       B.V.Integer.makestring expected_offset ^
			       ", length " ^
			       B.V.Integer.makestring (FoxWord32.wordToInt
						       len) ^
			       ", expecting " ^
			       B.V.Integer.makestring expected_len);
		  false)
	     end))

(*
---------------------------------------------------------------------
	11.	internal function check_out_of_order
*)

  fun check_out_of_order (Tcb.Tcb {out_of_order, ...}, []) =
       if Tcb.Q.size (! out_of_order) = 0 then true
       else (local_print ("check_out_of_order failed, something in queue");
	     false)
    | check_out_of_order (tcb as (Tcb.Tcb {out_of_order, ...}),
			  in_data :: rest) =
       case Tcb.Q.next (! out_of_order) of
	  NONE =>
	   (local_print ("check_out_of_order failed, empty queue");
	    false)
	| SOME (new_queue, first) =>
	   (case (in_data, first) of
	       (Tcb.In_Seg {seg = Tcb.Seg {seq = s1, len = l1, ...},
			    data = d1},
	        Tcb.In_Seg {seg = Tcb.Seg {seq = s2, len = l2, ...},
			    data = d2}) =>
	        (out_of_order := new_queue;
		 if s1 = s2 andalso l1 = l2 andalso same_segment (d1, d2) then
		  check_out_of_order (tcb, rest)
		 else
		  (local_print ("check_out_of_order failed");
		   false))
	     | _ =>
		(local_print ("Fast_In in check_out_of_order, failed");
		 false))

(*
---------------------------------------------------------------------
	12.	internal function check_receive
*)

  fun check_receive (expected_actions, expected_retransmits, expected_nxt,
		     expected_window, expected_out_of_order,
		     to_do, tcb, rcv_nxt, rcv_wnd) =
       let val to_do_test = check_to_do (! to_do, expected_actions)
           fun resend_string resend =
	        case Tcb.D.first resend of
		   NONE => ""
		 | SOME (rest, (out, time, resend)) =>
		    let val (seq, len) =
		          case out of
			     Tcb.Out_Seg {seg = Tcb.Seg {seq, len, ...},
					  ...} => (seq, len)
			   | Tcb.Fast_Out {seq, len, ...} => (seq, len)
			   | Tcb.Fast_Empty {seq, ...} => (seq, SW.n32 "0")
		    in "(" ^ FoxMakestring.word32 seq ^ "," ^
		       FoxMakestring.word32 len ^ ") " ^ resend_string rest
		    end
           fun resend_list_string [] = ""
	     | resend_list_string ((off, len) :: rest) =
	        ("(" ^ B.V.Integer.makestring off ^ "," ^
		 B.V.Integer.makestring len ^ ") " ^ resend_list_string rest)
	   val Tcb.Tcb {resend, ...} = tcb
	   val save_resend = ! resend
	   val retransmit_test =
	        if check_retransmit (tcb, expected_retransmits) then true
		else
		 let val Tcb.Tcb {resend, ...} = tcb
		 in local_print ("retransmit: actual [" ^
				 resend_string save_resend ^ "], expecting [" ^
				 resend_list_string expected_retransmits ^
				 "]");
                    false
		 end
	   val ooo_test = check_out_of_order (tcb, expected_out_of_order)
	   val rcv_nxt_test =
	        if ! rcv_nxt = (FoxWord32.intToWord expected_nxt) then true
		else (local_print ("rcv.nxt is " ^
				   FoxMakestring.word32 (! rcv_nxt) ^
				   ", expecting " ^
				   FoxMakestring.word32 (FoxWord32.intToWord
						     expected_nxt));
		      false)
	   val rcv_wnd_test =
	        if ! rcv_wnd = (FoxWord32.intToWord expected_window) then true
		else (local_print ("rcv.wnd is " ^
				   FoxMakestring.word32 (! rcv_wnd) ^
				   ", expecting " ^
				   FoxMakestring.word32 (FoxWord32.intToWord
						     expected_window));
		      false)
       in to_do_test andalso retransmit_test andalso ooo_test andalso
	  rcv_nxt_test andalso rcv_wnd_test
       end

(*
---------------------------------------------------------------------
	13.	internal function build_estab
*)

  fun build_estab (snd_una, snd_nxt, snd_wnd, rcv_nxt, mss, retransmits) =
       let val to_do_ref = ref (Tcb.Q.new ())
	   fun act _ = debug_print (fn _ => "timer expiration")
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
			      cwnd = ref 99999,
			      ssthresh = ref 99999,
			      unacked_segs = ref 1,
			      srtt = ref 16,
			      srtd = ref 17,
			      srto = ref 18,
			      wto = ref 19,
			      mss = ref (FoxWord16.intToWord mss),
			      resend = ref (Tcb.D.new ()),
			      queued = ref (Tcb.D.new ()),
			      out_of_order = ref (Tcb.Q.new ()),
			      timers = Tcb.empty_timer_set act,
			      to_do = ref (Tcb.Q.new ())}
       in build_retransmit (tcb, retransmits);
	  Tcb.Estab tcb
       end

(*
---------------------------------------------------------------------
	14.	function test_small
*)

  fun test_small () =
       let val s0 = build_estab (22000, 23001, 4096, 10000, 1460, [])
	   val in_seg = build_in (10000, 22999, 50, 128, true, NONE)
	   val s1 = Receive.receive (s0, in_seg)
	   val user_seg = build_segment (50, 10000)
	   fun fill n = FoxWord8.intToWord (n mod 256)
	   val out_ack = Tcb.Seg {seq = SW.n32 "23001", ack = SW.n32 "10050",
				  len = SW.n32 "0", wnd = SW.n32 "974",
				  up = SW.n32 "0", options = [],
				  syn_flag = false, fin_flag = false,
				  reset_flag = false, ack_flag = true,
				  push_flag = false, urgent_flag = false}
	   val actions = [Tcb.User_Data user_seg]
       in case s1 of
	     Tcb.Estab (tcb as (Tcb.Tcb {to_do, rcv_nxt, rcv_wnd, ...})) =>
	      check_receive (actions, [], 10050, 974, [],
			     to_do, tcb, rcv_nxt, rcv_wnd)
	   | _ =>
	      (local_print ("error in test_small, state is " ^
			    Tcb.state_string s1);
	       false)
       end

(*
---------------------------------------------------------------------
	15.	function test_urgent
*)

  fun test_urgent () =
       let val s0 = build_estab (22000, 23001, 4096, 10000, 1460, [])
	   val in_seg = build_in (10000, 22999, 50, 128, true, SOME 49)
	   val s1 = Receive.receive (s0, in_seg)
	   val user_seg = build_segment (50, 10000)
	   fun fill n = FoxWord8.intToWord (n mod 256)
	   val out_ack = Tcb.Seg {seq = SW.n32 "23001", ack = SW.n32 "10050",
				  len = SW.n32 "0", wnd = SW.n32 "974",
				  up = SW.n32 "0", options = [],
				  syn_flag = false, fin_flag = false,
				  reset_flag = false, ack_flag = true,
				  push_flag = false, urgent_flag = false}
	   val actions = [Tcb.Urgent_Data user_seg]
       in case s1 of
	     Tcb.Estab (tcb as (Tcb.Tcb {to_do, rcv_nxt, rcv_wnd, ...})) =>
	      check_receive (actions, [], 10050, 974, [],
			     to_do, tcb, rcv_nxt, rcv_wnd)
	   | _ =>
	      (local_print ("error in test_urgent, state is " ^
			    Tcb.state_string s1);
	       false)
       end

(*
---------------------------------------------------------------------
	16.	function test_part_urgent
*)

  fun test_part_urgent () =
       let val s0 = build_estab (22000, 23001, 4096, 10000, 1460, [])
	   val in_seg = build_in (10000, 22999, 50, 128, true, SOME 24)
	   val s1 = Receive.receive (s0, in_seg)
	   val urgent_seg = build_segment (25, 10000)
	   val user_seg = build_segment (25, 10025)
	   fun fill n = FoxWord8.intToWord (n mod 256)
	   val out_ack = Tcb.Seg {seq = SW.n32 "23001", ack = SW.n32 "10050",
				  len = SW.n32 "0", wnd = SW.n32 "974",
				  up = SW.n32 "0", options = [],
				  syn_flag = false, fin_flag = false,
				  reset_flag = false, ack_flag = true,
				  push_flag = false, urgent_flag = false}
	   val actions = [Tcb.Urgent_Data urgent_seg, Tcb.User_Data user_seg]
       in case s1 of
	     Tcb.Estab (tcb as (Tcb.Tcb {to_do, rcv_nxt, rcv_wnd, ...})) =>
	      check_receive (actions, [], 10050, 974, [],
			     to_do, tcb, rcv_nxt, rcv_wnd)
	   | _ =>
	      (local_print ("error in test_urgent, state is " ^
			    Tcb.state_string s1);
	       false)
       end

(*
---------------------------------------------------------------------
	17.	function test_beyond_window

	Make sure receive truncates a segment it receives that
	exceeds the allowed window length.
*)

  fun test_beyond_window () =
       let val s0 = build_estab (22000, 23001, 4096, 10000, 1460,
				 [(22000, 990), (22990, 10)])
	   val _ = case s0 of
	              Tcb.Estab (Tcb.Tcb {unacked_segs, ...}) =>
		       unacked_segs := 0
		    | _ => ()
	   val in_seg = build_in (10000, 22989, 1200, 128, false, NONE)
	   val s1 = Receive.receive (s0, in_seg)
	   val user_seg = build_segment (1024, 10000)
	   val actions = [Tcb.User_Data user_seg,
			  Tcb.Complete_Send (SW.n32 "22989")]
	   val retransmits = [(22000, 990), (22990, 10)]
       in case s1 of
	     Tcb.Estab (tcb as (Tcb.Tcb {to_do, rcv_nxt, rcv_wnd, ...})) =>
	      check_receive (actions, retransmits, 11024, 0, [],
			     to_do, tcb, rcv_nxt, rcv_wnd)
	   | _ =>
	      (local_print ("error in test_beyond_window, state is " ^
			    Tcb.state_string s1);
	       false)
       end

(*
---------------------------------------------------------------------
	18.	function test_large_segment

	Make sure receive doesn't die on a segment it receives that
	exceeds the allowed segment length.
*)

  fun test_large_segment () =
       let val s0 = build_estab (22000, 23001, 4096, 10000, 1460,
				 [(22000, 990), (22990, 10)])
	   val _ = case s0 of
	              Tcb.Estab (Tcb.Tcb {rcv_wnd, ...}) =>
		       rcv_wnd := SW.n32 "10000"
		    | _ => ()
	   val in_seg = build_in (10000, 23001, 10000, 128, true, NONE)
	   val s1 = Receive.receive (s0, in_seg)
	   val user_seg = build_segment (10000, 10000)
	   fun fill n = FoxWord8.intToWord (n mod 256)
	   val actions = [Tcb.User_Data user_seg,
			  Tcb.Complete_Send (SW.n32 "23001")]
       in case s1 of
	     Tcb.Estab (tcb as (Tcb.Tcb {to_do, rcv_nxt, rcv_wnd, ...})) =>
	      check_receive (actions, [], 20000, 0, [],
			     to_do, tcb, rcv_nxt, rcv_wnd)
	   | _ =>
	      (local_print ("error in test_large_segment, state is " ^
			    Tcb.state_string s1);
	       false)
       end

(*
---------------------------------------------------------------------
	19.	function test_zero_window
*)

  fun test_zero_window () =
       let val s0 = build_estab (22000, 23001, 4096, 10000, 1460,
				 [(22000, 990), (22990, 10)])
	   val _ = case s0 of
	              Tcb.Estab (Tcb.Tcb {rcv_wnd, ...}) =>
		       rcv_wnd := SW.n32 "0"
		    | _ => ()
	   val in_seg = build_in (10000, 22999, 0, 128, false, SOME 0)
	   val s1 = Receive.receive (s0, in_seg)
	   val actions = [Tcb.Complete_Send (SW.n32 "22999")]
	   val retransmits = [(22990, 10)]
       in case s1 of
	     Tcb.Estab (tcb as (Tcb.Tcb {to_do, rcv_nxt, rcv_wnd, ...})) =>
	      check_receive (actions, retransmits, 10000, 0, [],
			     to_do, tcb, rcv_nxt, rcv_wnd)
	   | _ =>
	      (local_print ("error in test_zero_window, state is " ^
			    Tcb.state_string s1);
	       false)
       end

(*
---------------------------------------------------------------------
	20.	function test_probe_window
*)

  fun test_probe_window () =
       let val s0 = build_estab (22000, 23001, 4096, 10000, 1460,
				 [(22000, 990), (22990, 10)])
	   val _ = case s0 of
	              Tcb.Estab (Tcb.Tcb {rcv_wnd, ...}) =>
		       rcv_wnd := SW.n32 "0"
		    | _ => ()
	   val in_seg = build_in (10000, 22999, 100, 128, false, NONE)
	   val s1 = Receive.receive (s0, in_seg)
	   val out_ack = Tcb.Seg {seq = SW.n32 "23001",
				  ack = SW.n32 "10000", len = SW.n32 "0",
				  wnd = SW.n32 "0", up = SW.n32 "0",
				  options = [],
				  syn_flag = false, fin_flag = false,
				  reset_flag = false, ack_flag = true,
				  push_flag = false, urgent_flag = false}
	   fun fill n = FoxWord8.intToWord (n mod 256)
	   val actions = [Tcb.Send_Segment
			  (Tcb.Out_Seg {seg = out_ack,
					data = B.Dyn_Array.empty},
			   NONE),
			  Tcb.Complete_Send (SW.n32 "22999")]
	   val retransmits = [(22990, 10)]
       in case s1 of
	     Tcb.Estab (tcb as (Tcb.Tcb {to_do, rcv_nxt, rcv_wnd, ...})) =>
	      check_receive (actions, retransmits, 10000, 0, [],
			     to_do, tcb, rcv_nxt, rcv_wnd)
	   | _ =>
	      (local_print ("error in test_probe_window, state is " ^
			    Tcb.state_string s1);
	       false)
       end

(*
---------------------------------------------------------------------
	21.	function test_zero_packet
*)

  fun test_zero_packet () =
       let val s0 = build_estab (22000, 23001, 4096, 10000, 1460,
				 [(22000, 990), (22990, 10)])
	   val in_seg = build_in (10000, 23001, 0, 128, false, NONE)
	   val s1 = Receive.receive (s0, in_seg)
	   val actions = [Tcb.Complete_Send (SW.n32 "23001")]
       in case s1 of
	     Tcb.Estab (tcb as (Tcb.Tcb {to_do, rcv_nxt, rcv_wnd, ...})) =>
	      check_receive (actions, [], 10000, 1024, [],
			     to_do, tcb, rcv_nxt, rcv_wnd)
	   | _ =>
	      (local_print ("error in test_zero_packet, state is " ^
			    Tcb.state_string s1);
	       false)
       end

(*
---------------------------------------------------------------------
	22.	function test_early_packet

	Make sure that a packet whose segment number is greater
	than rcv_nxt is queued.

	Also make sure that its ack is used immediately.
*)

  fun test_early_packet () =
       let val s0 = build_estab (22000, 23001, 4096, 10000, 1460,
				 [(22000, 990), (22990, 10)])
	   val in_seg = build_in (10100, 23001, 100, 128, false, NONE)
	   val s1 = Receive.receive (s0, in_seg)
	   val actions = [Tcb.Complete_Send (SW.n32 "23001")]
       in case s1 of
	     Tcb.Estab (tcb as (Tcb.Tcb {to_do, rcv_nxt, rcv_wnd, ...})) =>
	      check_receive (actions, [], 10000, 1024, [in_seg],
			     to_do, tcb, rcv_nxt, rcv_wnd)
	   | _ =>
	      (local_print ("error in test_early_packet, state is " ^
			    Tcb.state_string s1);
	       false)
       end

(*
---------------------------------------------------------------------
	23.	function test_late_packet

	Make sure that we requeue any packet that came out of order.
*)

  fun test_late_packet () =
       let val s0 = build_estab (22000, 23001, 4096, 10000, 1460, [])
	   val _ = case s0 of
	              Tcb.Estab (Tcb.Tcb {unacked_segs, ...}) =>
		       unacked_segs := 0
		    | _ => ()
	   val in_seg1 = build_in (10000, 23001, 100, 128, false, NONE)
	   val user_seg = build_segment (100, 10000)
	   val in_seg2 = build_in (10100, 23001, 100, 128, false, NONE)
	   val s1 = Receive.receive (s0, in_seg2)
	   val s2 = Receive.receive (s1, in_seg1)
	   val actions = [Tcb.Process_Data in_seg2,
			  Tcb.User_Data user_seg]
       in case s2 of
	     Tcb.Estab (tcb as (Tcb.Tcb {to_do, rcv_nxt, rcv_wnd, ...})) =>
	      check_receive (actions, [], 10100, 924, [],
			     to_do, tcb, rcv_nxt, rcv_wnd)
	   | _ =>
	      (local_print ("error in test_late_packet, state is " ^
			    Tcb.state_string s1);
	       false)
       end

(*
---------------------------------------------------------------------
	24.	function run_tests
*)

  fun run_tests () =
       (B.Test.test ("small", test_small);
        B.Test.test ("urgent", test_urgent);
        B.Test.test ("part urgent", test_part_urgent);
        B.Test.test ("beyond window", test_beyond_window);
        B.Test.test ("large segment", test_large_segment);
        B.Test.test ("zero window", test_zero_window);
        B.Test.test ("probe window", test_probe_window);
        B.Test.test ("zero packet", test_zero_packet);
        B.Test.test ("early packet", test_early_packet);
        B.Test.test ("late packet", test_late_packet);
        ())

(*
---------------------------------------------------------------------
	25.	function run
*)

  fun run () =
       B.Test.tests ("TcpReceive", 10, run_tests)

  val _ = if ! B.Debug.do_tests then run () else ()

 end (* struct *)

(*
---------------------------------------------------------------------
		26.	structure Test_Tcp_Receive
*)

structure Test_Tcp_Receive = Test_Tcp_Receive (structure B = Fox_Basis
					       val debug_level = NONE)
