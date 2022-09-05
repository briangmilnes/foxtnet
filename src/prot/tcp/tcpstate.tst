(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edo Biagioni (esb@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

	i.	Abstract

	tcpstate.tst: simple conformance-to-RFC test for the
	TCP state machine.

---------------------------------------------------------------------

	ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor Test_Tcp_State
	2.	function same_action
	3.	function check_same_set
	4.	function check_to_do
	5.	value timer_set
	6.	function test_closed_to_listen
	7.	function test_listen_to_closed
	8.	function test_closed_to_syn_sent
	9.	function test_syn_sent_to_closed
	10.	function test_listen_to_syn_passive
	11.	function test_syn_passive_to_listen
	12.	internal function find_send 
	13.	internal function send_ack 
	14.	function test_syn_sent_to_syn_active 
	15.	function test_syn_passive_to_estab
	16.	function test_syn_active_to_estab
	17.	function test_syn_sent_to_estab
	18.	function test_syn_passive_to_fw1
	19.	function test_syn_active_to_fw1
	20.	internal function make_estab
	21.	function test_estab_to_fw1
	22.	function test_estab_to_close_wait
	23.	function test_fw1_to_time_wait
	24.	function test_fw1_to_fw2
	25.	function test_fw1_to_closing
	26.	function test_close_wait_to_last_ack
	27.	function test_fw2_to_time_wait
	28.	function test_closing_to_time_wait
	29.	function test_last_ack_to_closed
	30.	function test_time_wait_to_closed
	31.	function run_tests
	32.	function run
	33.	structure Test_Tcp_State

---------------------------------------------------------------------
	iii.	RCS Log

$Log: tcpstate.tst,v $
Revision 1.25  1995/03/12  17:52:02  esb
adapted to new trace.sig.

Revision 1.24  1995/03/10  03:47:37  esb
adapted to new vendor.sig.

Revision 1.23  1995/03/07  23:57:08  esb
updated tracing.

Revision 1.22  1995/02/21  13:05:02  esb
upgraded for SML/NJ 1.07.

Revision 1.21  1995/01/18  21:11:39  esb
added a scheduler reset so it will work multiple times.

Revision 1.20  1994/10/25  16:34:23  esb
set do_prints to false.

Revision 1.19  1994/10/11  15:43:04  cline
Upgraded to work with SML/NJ 1.05b

Revision 1.18  1994/08/24  22:20:27  esb
added complete_sends in the closing states.

Revision 1.17  1994/08/18  20:32:06  esb
adapted to new actions, support for status messages.

Revision 1.16  1994/08/17  16:28:26  esb
minor tuning and bug fixes.

Revision 1.15  1994/08/12  06:27:15  esb
converted to the new protocol signature.

Revision 1.14  1994/07/11  17:51:43  esb
small change

Revision 1.13  1994/07/01  02:32:53  danwang
Moved control structures into Fox_Basis.

Revision 1.12  1994/06/16  16:42:04  danwang
Updated to use functorized Fox_Basis

Revision 1.11  1994/06/13  23:13:13  esb
moved timer implementation from tcpaction to tcptcb.

Revision 1.10  1994/05/10  08:09:07  esb
added clauses for Fast_Out and Fast_Push to same_segment.

Revision 1.9  94/04/15  03:19:40  esb
Fixed an incorrect state that had not been detected due to a bug.

Revision 1.8  94/04/06  23:18:40  esb
adapted to new receive_packet interface.

Revision 1.7  94/02/17  01:12:26  esb
interface changes.

Revision 1.6  94/02/14  14:26:57  esb
now ignores log messages.

Revision 1.5  94/01/30  21:02:09  esb
added checking for Tcb.Complete_Send in same_action.

Revision 1.4  1994/01/28  01:30:34  esb
minor interface changes.

Revision 1.3  1994/01/19  21:31:58  esb
adapted to new interface.

Revision 1.2  1994/01/09  03:20:46  esb
first functional version.

Revision 1.1  1993/11/11  05:31:56  esb
Initial revision


*)

(*
---------------------------------------------------------------------
	1.	functor Test_Tcp_State

	All page numbers refer to RFC 793 unless otherwise indicated.
 *)

functor Test_Tcp_State (structure B: FOX_BASIS
			val debug_level: int ref option): TEST_STRUCTURE =
 struct
  structure Trace = Trace (structure V = B.V
			   val debug_level = debug_level
			   val module_name = "tcpstate.tst")
  val local_print = Trace.local_print
  val debug_print = Trace.debug_print

  structure Log = Tcp_Log (structure B = B)

  structure Tcb = Tcp_Tcb (structure B = B
			   structure Tcp_Log = Log)

  structure Retransmit = Tcp_Retransmit (structure Tcp_Tcb = Tcb
					 structure B = B)

  structure State = Tcp_State (structure Tcp_Tcb = Tcb
			       structure Retransmit = Retransmit
			       structure B = B
			       val debug_level = debug_level)

  structure Send = Tcp_Send (structure Tcp_Tcb = Tcb
			     structure Retransmit = Retransmit
			     structure B = B
			     val debug_level = debug_level)

  structure Receive = Tcp_Receive (structure Tcp_Tcb = Tcb
				   structure Retransmit = Retransmit
				   val wait_ms = 1000
				   val ack_timeout_ms = 100
				   structure B = B
				   val debug_level = debug_level)

(*
---------------------------------------------------------------------
	2.	function same_action
*)

  fun same_action (Tcb.User_Error s1, Tcb.User_Error s2) = s1 = s2
    | same_action (Tcb.Send_Segment (Tcb.Fast_Out {seq, ack, len,
						   wnd, data}, _),
		   Tcb.Send_Segment (segment, _)) =
       let val new_seg = Tcb.Seg {seq = seq, ack = ack, len = len, wnd = wnd,
				  up = SW.n32 "0", options = [],
				  syn_flag = false,
				  fin_flag = false, ack_flag = true,
				  reset_flag = false, push_flag = true,
				  urgent_flag = false}
	   val new_out = Tcb.Out_Seg {seg = new_seg, data = data}
       in same_action (Tcb.Send_Segment (new_out, NONE),
		       Tcb.Send_Segment (segment, NONE))
       end
    | same_action (Tcb.Send_Segment (Tcb.Fast_Empty {seq, ack, wnd}, _),
		   Tcb.Send_Segment (segment, _)) =
       let val new_seg = Tcb.Seg {seq = seq, ack = ack,
				  len = SW.n32 "0", wnd = wnd,
				  up = SW.n32 "0", options = [],
				  syn_flag = false,
				  fin_flag = false, ack_flag = true,
				  reset_flag = false, push_flag = false,
				  urgent_flag = false}
	   val new_out = Tcb.Out_Seg {seg = new_seg, data = B.Dyn_Array.new 0}
       in same_action (Tcb.Send_Segment (new_out, NONE),
		       Tcb.Send_Segment (segment, NONE))
       end
    | same_action (Tcb.Send_Segment (segment, _),
		   Tcb.Send_Segment (Tcb.Fast_Out {seq, ack, len,
						   wnd, data}, _)) =
       let val new_seg = Tcb.Seg {seq = seq, ack = ack, len = len, wnd = wnd,
				  up = SW.n32 "0", options = [],
				  syn_flag = false,
				  fin_flag = false, ack_flag = true,
				  reset_flag = false, push_flag = true,
				  urgent_flag = false}
	   val new_out = Tcb.Out_Seg {seg = new_seg, data = data}
       in same_action (Tcb.Send_Segment (segment, NONE),
		       Tcb.Send_Segment (new_out, NONE))
       end
    | same_action (Tcb.Send_Segment (segment, _),
		   Tcb.Send_Segment (Tcb.Fast_Empty {seq, ack, wnd}, _)) =
       let val new_seg = Tcb.Seg {seq = seq, ack = ack, len = SW.n32 "0",
				  wnd = wnd,
				  up = SW.n32 "0", options = [],
				  syn_flag = false,
				  fin_flag = false, ack_flag = true,
				  reset_flag = false, push_flag = false,
				  urgent_flag = false}
	   val new_out = Tcb.Out_Seg {seg = new_seg, data = B.Dyn_Array.new 0}
       in same_action (Tcb.Send_Segment (segment, NONE),
		       Tcb.Send_Segment (new_out, NONE))
       end
    | same_action (Tcb.Send_Segment (Tcb.Out_Seg {seg = seg1, data = d1}, _),
		   Tcb.Send_Segment (Tcb.Out_Seg {seg = seg2, data = d2}, _)) =
       let val Tcb.Seg {seq = seq1, ack = ack1, len = len1, wnd = wnd1,
			up = up1, options = options1, syn_flag = syn1,
			fin_flag = fin1, ack_flag = acf1,
			reset_flag = rst1, push_flag = psh1,
			urgent_flag = urg1} = seg1
	   val Tcb.Seg {seq = seq2, ack = ack2, len = len2, wnd = wnd2,
			up = up2, options = options2, syn_flag = syn2,
			fin_flag = fin2, ack_flag = acf2,
			reset_flag = rst2, push_flag = psh2,
			urgent_flag = urg2} = seg2
	   val t1 = B.Dyn_Array.size d1 = B.Dyn_Array.size d2
	   val t2 = seq1 = seq2
	   val t3 = not acf1 orelse ack1 = ack2
	   val t4 = len1 = len2
	   val t5 = wnd1 = wnd2
	   val t6 = not urg1 orelse up1 = up2
	   val t7 = syn1 = syn2 andalso fin1 = fin2 andalso
		    acf1 = acf2 andalso rst1 = rst2 andalso
		    urg1 = urg2 andalso psh1 = psh2
	   val result = t1 andalso t2 andalso t3 andalso t4 andalso
	                t5 andalso t6 andalso t7
	   val ack_string = if not acf1 then ""
		            else (FoxMakestring.word32 ack1 ^ "/" ^
				  FoxMakestring.word32 ack2 ^ ", ")
	   val urg_string = if not urg1 then "\n"
			    else (FoxMakestring.word32 up1 ^ "/" ^
				  FoxMakestring.word32 up2 ^ ",\n")
	   val res_string =
	     "(" ^ FoxMakestring.bool t1 ^ " & " ^
	           FoxMakestring.bool t2 ^ " & " ^
	           FoxMakestring.bool t3 ^ " & " ^
		   FoxMakestring.bool t4 ^ " & " ^
		   FoxMakestring.bool t5 ^ " & " ^
		   FoxMakestring.bool t6 ^ " & " ^
		   FoxMakestring.bool t7 ^ ")\n"
       in if result then true
	  else
	   (local_print
	    ("size1 = " ^ makestring (B.Dyn_Array.size d1) ^ ", " ^
	     "size2 = " ^ makestring (B.Dyn_Array.size d2) ^ "\n" ^
	     "seg1 = " ^ Tcb.segment_string seg1 ^ ", " ^
	     "seg2 = " ^ Tcb.segment_string seg2 ^ "\n" ^
	     FoxMakestring.word32 seq1 ^ "/" ^
	     FoxMakestring.word32 seq2 ^ ", " ^
	     ack_string ^
	     makestring len1 ^ "/" ^       makestring len2 ^ ", " ^
	     FoxMakestring.word32 wnd1 ^ "/" ^
	     FoxMakestring.word32 wnd2 ^ ", " ^
	     urg_string ^
	     FoxMakestring.bool syn1 ^ "/" ^  FoxMakestring.bool syn2 ^ ", " ^
	     FoxMakestring.bool fin1 ^ "/" ^  FoxMakestring.bool fin2 ^ ", " ^
	     FoxMakestring.bool acf1 ^ "/" ^  FoxMakestring.bool acf2 ^ ", " ^
	     FoxMakestring.bool rst1 ^ "/" ^  FoxMakestring.bool rst2 ^ ", " ^
	     FoxMakestring.bool urg1 ^ "/" ^  FoxMakestring.bool urg2 ^ ", " ^
	     FoxMakestring.bool psh1 ^ "/" ^  FoxMakestring.bool psh2 ^ "\n" ^
	     res_string);
	    false)
       end (* let *)
    | same_action (Tcb.User_Data packet1, Tcb.User_Data packet2) =
       B.Dyn_Array.size packet1 = B.Dyn_Array.size packet2
    | same_action (Tcb.Urgent_Data packet1, Tcb.Urgent_Data packet2) =
       B.Dyn_Array.size packet1 = B.Dyn_Array.size packet2
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
	3.	function check_same_set
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
	4.	function check_to_do
*)

  fun check_to_do (to_do, expected) =
       let fun build (queue, res) =
	        case Tcb.Q.next queue of
		   NONE => rev res
		 | SOME (new_queue, next) =>
		    build (new_queue, next :: res)
	   fun no_logs [] = []
	     | no_logs (Tcb.Log_Event _ :: rest) = no_logs rest
	     | no_logs (head :: rest) = head :: (no_logs rest)
	   fun member ([], _) = false
	     | member (head :: rest, object) =
	        if same_action (head, object) then true
		else member (rest, object)
	   val to_do_list = no_logs (build (to_do, []))
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
	5.	value timer_set
*)

  fun print_timer_expiration timer =
       local_print ("Timer_Expiration " ^ Tcb.timer_string timer)

  val timer_set = Tcb.empty_timer_set print_timer_expiration

(*
---------------------------------------------------------------------
	6.	function test_closed_to_listen
*)

  fun test_closed_to_listen () =
       let val state0 = Tcb.Closed (ref (Tcb.Q.new ()), timer_set)
           val state1 = State.passive_open (state0, 4096, 1460)
       in case state1 of
	     Tcb.Listen (Tcb.Tcb {to_do, ...}, mss) =>
	      check_to_do (! to_do, []) andalso mss = 1460
	   | _ => false
       end

(*
---------------------------------------------------------------------
	7.	function test_listen_to_closed
*)

  fun test_listen_to_closed () =
       let val state0 = Tcb.Closed (ref (Tcb.Q.new ()), timer_set)
           val state1 = State.passive_open (state0, 4096, 1460)
           val state2 = State.close state1
       in case state2 of
	     Tcb.Closed (to_do, _) => check_to_do (! to_do, [Tcb.Delete_Tcb])
	   | _ => false
       end

(*
---------------------------------------------------------------------
	8.	function test_closed_to_syn_sent
*)

  fun test_closed_to_syn_sent () =
       let val state0 = Tcb.Closed (ref (Tcb.Q.new ()), timer_set)
           val state1 = State.active_open (state0, 4096, 1460)
       in case state1 of
	     Tcb.Syn_Sent (Tcb.Tcb {iss, to_do, ...}, max_size) =>
	      let val opts = [Tcb.Max_Segment {size = SW.n16 "1460"}]
	          val empty_data = B.Dyn_Array.new 0
	          val expected_seg =
	                Tcb.Seg {seq = iss, ack = SW.n32 "0", len = SW.n32 "0",
				 wnd = SW.n32 "4096", up = SW.n32 "0",
				 options = opts,
				 syn_flag = true, fin_flag = false,
				 reset_flag = false, ack_flag = false,
				 push_flag = false, urgent_flag = false}
		  val expected_send = (Tcb.Out_Seg {seg = expected_seg,
						    data = empty_data}, NONE)
	      in check_to_do (! to_do,
			      [Tcb.Send_Segment expected_send])
	      end
	   | _ => false
       end

(*
---------------------------------------------------------------------
	9.	function test_syn_sent_to_closed
*)

  fun test_syn_sent_to_closed () =
       let val state0 = Tcb.Closed (ref (Tcb.Q.new ()), timer_set)
           val state1 = State.active_open (state0, 4096, 1460)
	   fun make_list to_do =
	        case Tcb.Q.next to_do of
		   NONE => []
		 | SOME (rest, head) => head :: make_list rest
	   val original_to_do =
	        (Tcb.Delete_Tcb ::
		 (case state1 of
		    Tcb.Syn_Sent (Tcb.Tcb {to_do, ...}, _) =>
		     make_list (! to_do)
		   | _ => []))
           val state2 = State.close state1
       in case state2 of
	     Tcb.Closed (to_do, _) => check_to_do (! to_do, original_to_do)
	   | _ => false
       end

(*
---------------------------------------------------------------------
	10.	function test_listen_to_syn_passive
*)

  fun test_listen_to_syn_passive () =
       let val state0 = Tcb.Closed (ref (Tcb.Q.new ()), timer_set)
           val state1 = State.passive_open (state0, 4096, 1460)
	   val iss_in = SW.n32 "123456"
	   val opts_in = [Tcb.Max_Segment {size = SW.n16 "536"}]
	   val seg = Tcb.Seg {seq = iss_in, ack = SW.n32 "333",
			      len = SW.n32 "0", wnd = SW.n32 "123",
			      up = SW.n32 "777", options = opts_in,
			      syn_flag = true, fin_flag = false,
			      reset_flag = false, ack_flag = false,
			      push_flag = true, urgent_flag = true}
	   val in_seg = Tcb.In_Seg {seg = seg, data = B.Dyn_Array.empty}
	   val opts = [Tcb.Max_Segment {size = SW.n16 "1460"}]
	   val empty_data = B.Dyn_Array.new 0
           val state2 = Receive.receive (state1, in_seg)
       in case state2 of
	     Tcb.Syn_Passive (Tcb.Tcb {iss, to_do, ...}, max_size) =>
	      let val expected_seg =
	                Tcb.Seg {seq = iss,
				 ack = FoxWord32.+ (iss_in, SW.n32 "1"),
				 len = SW.n32 "0", wnd = SW.n32 "4096",
				 up = SW.n32 "0", options = opts,
				 syn_flag = true, fin_flag = false,
				 reset_flag = false, ack_flag = true,
				 push_flag = false, urgent_flag = false}
		  val expected_send = (Tcb.Out_Seg {seg = expected_seg,
						    data = empty_data}, NONE)
	      in check_to_do (! to_do, [Tcb.Send_Segment expected_send])
	         andalso max_size = 1460
	      end
	   | _ => false
       end

(*
---------------------------------------------------------------------
	11.	function test_syn_passive_to_listen
*)

  fun test_syn_passive_to_listen () =
       let val state0 = Tcb.Closed (ref (Tcb.Q.new ()), timer_set)
           val state1 = State.passive_open (state0, 4096, 1460)
	   val iss_in = SW.n32 "123456"
	   val opts_in = [Tcb.Max_Segment {size = SW.n16 "536"}]
	   val seg = Tcb.Seg {seq = iss_in, ack = SW.n32 "333",
			      len = SW.n32 "0", wnd = SW.n32 "123",
			      up = SW.n32 "777", options = opts_in,
			      syn_flag = true, fin_flag = false,
			      reset_flag = false, ack_flag = false,
			      push_flag = true, urgent_flag = true}
	   val in_seg = Tcb.In_Seg {seg = seg, data = B.Dyn_Array.empty}
           val state2 = Receive.receive (state1, in_seg)
	   val rst = Tcb.Seg {seq = FoxWord32.+ (iss_in, SW.n32 "1"),
			      ack = SW.n32 "0",
			      len = SW.n32 "0", wnd = SW.n32 "0",
			      up = SW.n32 "0", options = [],
			      syn_flag = false, fin_flag = false,
			      reset_flag = true, ack_flag = false,
			      push_flag = false, urgent_flag = false}
	   val in_rst = Tcb.In_Seg {seg = rst, data = B.Dyn_Array.empty}
	   val _ = case state2 of
	              Tcb.Syn_Passive (Tcb.Tcb {to_do, ...}, _) =>
		       to_do := Tcb.Q.new ()
		    | _ => ()
           val state3 = Receive.receive (state2, in_rst)
       in case state3 of
	     Tcb.Listen (Tcb.Tcb {iss, to_do, ...}, mss) =>
	      check_to_do (! to_do, []) andalso mss = 1460
	   | _ => false
       end

(*
---------------------------------------------------------------------
	12.	internal function find_send 
*)

  fun find_send queue =
       case Tcb.Q.next queue of
	  NONE => NONE
	| SOME (new_queue, Tcb.Send_Segment (segment, _)) =>
	   (case find_send new_queue of
	       NONE =>
		(case segment of
		    Tcb.Out_Seg {data, seg} =>
		     let val Tcb.Seg {seq, ack, len, wnd, up, options,
				      syn_flag, fin_flag, ...} = seg
		     in SOME (seq, ack, len, wnd, up, options,
			      syn_flag, fin_flag)
		     end
	          | Tcb.Fast_Out {seq, ack, len, wnd, ...} =>
		     SOME (seq, ack, len, wnd, SW.n32 "0", [], false, false)
	          | Tcb.Fast_Empty {seq, ack, wnd} =>
		     SOME (seq, ack, SW.n32 "0", wnd, SW.n32 "0",
			   [], false, false))
	     | x => x)
	| SOME (new_queue, _) => find_send new_queue

(*
---------------------------------------------------------------------
	13.	internal function send_ack 

	Compute the ack value for the last segment sent in the to_do list.
	Also clear the to_do list (but only if you find a send).
*)

  fun send_ack (tcb as (Tcb.Tcb {to_do, resend, ...})) =
       (case find_send (! to_do) of
	   NONE => (local_print "no send in to-do list"; NONE)
	 | SOME (seq, ack, len, wnd, up, options, syn_flag, fin_flag) =>
	    (resend := Tcb.D.new ();
	     to_do := Tcb.Q.new ();
	     let val new_len = if syn_flag orelse fin_flag then
	                        FoxWord32.+ (len, SW.n32 "1")
			       else len
		 val ack = FoxWord32.+ (seq, new_len)
	     in debug_print (fn _ => "send_ack returns " ^
			     FoxMakestring.word32 ack);
	        SOME ack
	     end))

(*
---------------------------------------------------------------------
	14.	function test_syn_sent_to_syn_active 
*)

  fun test_syn_sent_to_syn_active () =
       let val state0 = Tcb.Closed (ref (Tcb.Q.new ()), timer_set)
           val state1 = State.active_open (state0, 4096, 1460)
	   val iss_in = SW.n32 "1"
	   val expected_actions =
	        case state1 of
		   Tcb.Syn_Sent (tcb as (Tcb.Tcb {to_do, resend, ...}), _) =>
		    (case find_send (! to_do) of
		        NONE => []
		      | SOME (seq, ack, len, wnd, up, options, _, _) =>
			 (resend := Tcb.D.new ();
			  to_do := Tcb.Q.new ();
			  let val new_ack = FoxWord32.+ (iss_in, SW.n32 "1")
			      val seg =
			       Tcb.Seg {seq = seq, ack = new_ack, len = len,
					wnd = wnd, up = up, options = options,
					syn_flag = true, fin_flag = false,
					reset_flag = false, ack_flag = true,
					push_flag = false, urgent_flag = false}
			      val data = B.Dyn_Array.new 0
			      val out = (Tcb.Out_Seg {seg = seg, data = data},
					 NONE)
			      val actions = [Tcb.Send_Segment out]
			  in actions
			  end))
		 | _ => []
	   val opts_in = [Tcb.Max_Segment {size = SW.n16 "555"}]
	   val seg = Tcb.Seg {seq = iss_in, ack = SW.n32 "0", len = SW.n32 "0",
			      wnd = SW.n32 "999", up = SW.n32 "0",
			      options = opts_in,
			      syn_flag = true, fin_flag = false,
			      reset_flag = false, ack_flag = false,
			      push_flag = false, urgent_flag = false}
	   val in_seg = Tcb.In_Seg {seg = seg, data = B.Dyn_Array.empty}
           val state2 = Receive.receive (state1, in_seg)
       in case state2 of
	     Tcb.Syn_Active (Tcb.Tcb {to_do, ...}) =>
	      check_to_do (! to_do, expected_actions)
	   | _ => (local_print ("error: " ^ Tcb.state_string state2); false)
       end

(*
---------------------------------------------------------------------
	15.	function test_syn_passive_to_estab
*)

  fun test_syn_passive_to_estab () =
       let val state0 = Tcb.Closed (ref (Tcb.Q.new ()), timer_set)
           val state1 = State.passive_open (state0, 4096, 536)
	   val iss_in = SW.n32 "12345"
	   val opts_in = [Tcb.Max_Segment {size = SW.n16 "536"}]
	   val seg = Tcb.Seg {seq = iss_in, ack = SW.n32 "333",
			      len = SW.n32 "0", wnd = SW.n32 "123",
			      up = SW.n32 "777", options = opts_in,
			      syn_flag = true, fin_flag = false,
			      reset_flag = false, ack_flag = false,
			      push_flag = true, urgent_flag = true}
	   val in_seg = Tcb.In_Seg {seg = seg, data = B.Dyn_Array.empty}
           val state2 = Receive.receive (state1, in_seg)
	   val next_ack = case state2 of
		           Tcb.Syn_Passive (tcb, _) =>
			    (case send_ack tcb of NONE => SW.n32 "0"
			                        | SOME x => x)
			 | _ => SW.n32 "0"
	   val ack_seg = Tcb.Seg {seq = FoxWord32.+ (iss_in, SW.n32 "1"),
				  ack = next_ack,
				  len = SW.n32 "0", wnd = SW.n32 "256",
				  up = SW.n32 "0", options = [],
				  syn_flag = false, fin_flag = false,
				  reset_flag = false, ack_flag = true,
				  push_flag = true, urgent_flag = true}
	   val in_ack = Tcb.In_Seg {seg = ack_seg, data = B.Dyn_Array.empty}
           val state3 = Receive.receive (state2, in_ack)
	   val expected_list = [Tcb.Complete_Open true]
       in case state3 of
	     Tcb.Estab (Tcb.Tcb {to_do, ...}) =>
	      check_to_do (! to_do, expected_list)
	   | _ => (local_print ("error: " ^ Tcb.state_string state3); false)
       end

(*
---------------------------------------------------------------------
	16.	function test_syn_active_to_estab
*)

  fun test_syn_active_to_estab () =
       let val state0 = Tcb.Closed (ref (Tcb.Q.new ()), timer_set)
           val state1 = State.active_open (state0, 4096, 1460)
	   val iss_in = SW.n32 "1"
	   val opts_in = [Tcb.Max_Segment {size = SW.n16 "555"}]
	   val seg = Tcb.Seg {seq = iss_in, ack = SW.n32 "0",
			      len = SW.n32 "0", wnd = SW.n32 "999",
			      up = SW.n32 "0", options = opts_in,
			      syn_flag = true, fin_flag = false,
			      reset_flag = false, ack_flag = false,
			      push_flag = false, urgent_flag = false}
	   val in_seg = Tcb.In_Seg {seg = seg, data = B.Dyn_Array.empty}
	   val _ = case state1 of
	              Tcb.Syn_Sent (Tcb.Tcb {to_do, ...}, _) =>
		       to_do := Tcb.Q.new ()
		    | _ => local_print ("error: " ^ Tcb.state_string state1)
           val state2 = Receive.receive (state1, in_seg)
	   val next_ack = case state2 of
		             Tcb.Syn_Active tcb =>
			      (case send_ack tcb of NONE => SW.n32 "0"
			                          | SOME x => x)
			   | _ => SW.n32 "0"
	   val ack_seg = Tcb.Seg {seq = FoxWord32.+ (iss_in, SW.n32 "1"), ack = next_ack,
				  len = SW.n32 "0", wnd = SW.n32 "256",
				  up = SW.n32 "0", options = [],
				  syn_flag = false, fin_flag = false,
				  reset_flag = false, ack_flag = true,
				  push_flag = false, urgent_flag = false}
	   val in_ack = Tcb.In_Seg {seg = ack_seg, data = B.Dyn_Array.empty}
           val state3 = Receive.receive (state2, in_ack)
	   val expected_list = [Tcb.Complete_Open true]
       in case state3 of
	     Tcb.Estab (Tcb.Tcb {to_do, ...}) =>
	      check_to_do (! to_do, expected_list)
	   | _ => (local_print ("error: " ^ Tcb.state_string state3); false)
       end

(*
---------------------------------------------------------------------
	17.	function test_syn_sent_to_estab
*)

  fun test_syn_sent_to_estab () =
       let val state0 = Tcb.Closed (ref (Tcb.Q.new ()), timer_set)
           val state1 = State.active_open (state0, 4096, 1460)
	   val iss_in = SW.n32 "111"
	   val opts_in = [Tcb.Max_Segment {size = SW.n16 "555"}]
(* note: send_ack clears the resend queue *)
	   val next_ack = case state1 of
		             Tcb.Syn_Sent (tcb, _) =>
			      (case send_ack tcb of NONE => SW.n32 "0"
			                          | SOME x => x)
			   | _ => SW.n32 "0"
	   val seg = Tcb.Seg {seq = iss_in, ack = next_ack,
			      len = SW.n32 "0", wnd = SW.n32 "876",
			      up = SW.n32 "0", options = opts_in,
			      syn_flag = true, fin_flag = false,
			      reset_flag = false, ack_flag = true,
			      push_flag = false, urgent_flag = false}
	   val in_seg = Tcb.In_Seg {seg = seg, data = B.Dyn_Array.empty}
           val state2 = Receive.receive (state1, in_seg)
	   val out = Tcb.Seg {seq = next_ack,
			      ack = FoxWord32.+ (iss_in, SW.n32 "1"),
			      len = SW.n32 "0", wnd = SW.n32 "4096",
			      up = SW.n32 "0", options = [],
			      syn_flag = false, fin_flag = false,
			      reset_flag = false, ack_flag = true,
			      push_flag = false, urgent_flag = false}
	   val out_seg = (Tcb.Out_Seg {seg = out, data = B.Dyn_Array.empty},
			  NONE)
	   val expected_list = [Tcb.Send_Segment out_seg,
				Tcb.Complete_Open true]
       in case state2 of
	     Tcb.Estab (Tcb.Tcb {to_do, ...}) =>
	      check_to_do (! to_do, expected_list)
	   | _ => (local_print ("error: " ^ Tcb.state_string state2); false)
       end

(*
---------------------------------------------------------------------
	18.	function test_syn_passive_to_fw1
*)

  fun test_syn_passive_to_fw1 () =
       let val state0 = Tcb.Closed (ref (Tcb.Q.new ()), timer_set)
           val state1 = State.passive_open (state0, 4096, 536)
	   val iss_in = SW.n32 "12345"
	   val opts_in = [Tcb.Max_Segment {size = SW.n16 "536"}]
	   val seg = Tcb.Seg {seq = iss_in, ack = SW.n32 "333",
			      len = SW.n32 "0", wnd = SW.n32 "123",
			      up = SW.n32 "777", options = opts_in,
			      syn_flag = true, fin_flag = false,
			      reset_flag = false, ack_flag = false,
			      push_flag = true, urgent_flag = true}
	   val in_seg = Tcb.In_Seg {seg = seg, data = B.Dyn_Array.empty}
           val state2 = Receive.receive (state1, in_seg)
	   val next_seq = case state2 of
		             Tcb.Syn_Passive (tcb, _) =>
			      (case send_ack tcb of NONE => SW.n32 "0"
			                          | SOME x => x)
			   | _ => SW.n32 "0"
           val state3 = State.close state2
	   val fin_seg = Tcb.Seg {seq = next_seq,
				  ack = FoxWord32.+ (iss_in, SW.n32 "1"),
				  len = SW.n32 "0", wnd = SW.n32 "4096",
				  up = SW.n32 "0", options = [],
				  syn_flag = false, fin_flag = true,
				  reset_flag = false, ack_flag = true,
				  push_flag = false, urgent_flag = false}
	   val out_fin = (Tcb.Out_Seg {seg = fin_seg,
				       data = B.Dyn_Array.empty}, NONE)
	   val expected_list = [Tcb.Complete_Open true,
				Tcb.Send_Segment out_fin]
       in case state3 of
	     Tcb.Fin_Wait_1 (Tcb.Tcb {to_do, ...}, _) =>
	      check_to_do (! to_do, expected_list)
	   | _ => (local_print ("error: " ^ Tcb.state_string state3); false)
       end

(*
---------------------------------------------------------------------
	19.	function test_syn_active_to_fw1
*)

  fun test_syn_active_to_fw1 () =
       let val state0 = Tcb.Closed (ref (Tcb.Q.new ()), timer_set)
           val state1 = State.active_open (state0, 4096, 1460)
	   val iss_in = SW.n32 "1"
	   val opts_in = [Tcb.Max_Segment {size = SW.n16 "555"}]
	   val seg = Tcb.Seg {seq = iss_in, ack = SW.n32 "0",
			      len = SW.n32 "0", wnd = SW.n32 "999",
			      up = SW.n32 "0", options = opts_in,
			      syn_flag = true, fin_flag = false,
			      reset_flag = false, ack_flag = false,
			      push_flag = false, urgent_flag = false}
	   val in_seg = Tcb.In_Seg {seg = seg, data = B.Dyn_Array.empty}
	   val _ = case state1 of
	              Tcb.Syn_Sent (Tcb.Tcb {to_do, ...}, _) =>
		       to_do := Tcb.Q.new ()
		    | _ => local_print ("error: " ^ Tcb.state_string state1)
           val state2 = Receive.receive (state1, in_seg)
	   val next_seq = case state2 of
		             Tcb.Syn_Active tcb =>
			      (case send_ack tcb of NONE => SW.n32 "0"
			                          | SOME x => x)
			   | _ => SW.n32 "0"
           val state3 = State.close state2
	   val fin_seg = Tcb.Seg {seq = next_seq,
				  ack = FoxWord32.+ (iss_in, SW.n32 "1"),
				  len = SW.n32 "0", wnd = SW.n32 "4096",
				  up = SW.n32 "0", options = [],
				  syn_flag = false, fin_flag = true,
				  reset_flag = false, ack_flag = true,
				  push_flag = false, urgent_flag = false}
	   val out_fin = (Tcb.Out_Seg {seg = fin_seg,
				       data = B.Dyn_Array.empty}, NONE)
	   val expected_list = [Tcb.Complete_Open true,
				Tcb.Send_Segment out_fin]
       in case state3 of
	     Tcb.Fin_Wait_1 (Tcb.Tcb {to_do, ...}, _) =>
	      check_to_do (! to_do, expected_list)
	   | _ => (local_print ("error: " ^ Tcb.state_string state3); false)
       end

(*
---------------------------------------------------------------------
	20.	internal function make_estab

	make_estab returns a triple (state, seq, ack). Seq is the
	next sequence number to be used in talking to a Tcp with
	the given state, and ack is the next sequence number that
	will be used by a Tcp with the given state.
*)

  fun make_estab () =
       let val state0 = Tcb.Closed (ref (Tcb.Q.new ()), timer_set)
           val state1 = State.active_open (state0, 4096, 1460)
	   val iss_in = SW.n32 "111"
	   val opts_in = [Tcb.Max_Segment {size = SW.n16 "1460"}]
	   val next_ack = case state1 of
		             Tcb.Syn_Sent (tcb, _) =>
			      (case send_ack tcb of NONE => SW.n32 "0"
			                          | SOME x => x)
			   | _ => SW.n32 "0"
	   val seg = Tcb.Seg {seq = iss_in, ack = next_ack,
			      len = SW.n32 "0", wnd = SW.n32 "876",
			      up = SW.n32 "0", options = opts_in,
			      syn_flag = true, fin_flag = false,
			      reset_flag = false, ack_flag = true,
			      push_flag = false, urgent_flag = false}
	   val in_seg = Tcb.In_Seg {seg = seg, data = B.Dyn_Array.empty}
           val state2 = Receive.receive (state1, in_seg)
	   val next_ack = case state2 of
		             Tcb.Estab tcb =>
			      (case send_ack tcb of NONE => SW.n32 "0"
			                          | SOME x => x)
			   | _ => SW.n32 "0"
	   val _ = case state2 of
	              Tcb.Estab (tcb as (Tcb.Tcb {to_do, queued,
						  resend, ...})) =>
		       (resend := Tcb.D.new ();
			to_do := Tcb.Q.new ();
			queued := Tcb.D.new ())
		    | _ =>
		       local_print ("error in make_estab, final state is " ^
				    Tcb.state_string state2);
       in (state2, FoxWord32.+ (iss_in, SW.n32 "1"), next_ack)
       end

(*
---------------------------------------------------------------------
	21.	function test_estab_to_fw1
*)

  fun test_estab_to_fw1 () =
       let val (state0, next_seq, next_ack) = make_estab ()
           val state1 = State.close state0
	   val fin_seg = Tcb.Seg {seq = next_ack, ack = next_seq,
				  len = SW.n32 "0", wnd = SW.n32 "4096",
				  up = SW.n32 "0", options = [],
				  syn_flag = false, fin_flag = true,
				  reset_flag = false, ack_flag = true,
				  push_flag = false, urgent_flag = false}
	   val out_fin = (Tcb.Out_Seg {seg = fin_seg,
				       data = B.Dyn_Array.empty}, NONE)
	   val expected_list = [Tcb.Send_Segment out_fin]
       in case state1 of
	     Tcb.Fin_Wait_1 (Tcb.Tcb {to_do, ...}, _) =>
	      check_to_do (! to_do, expected_list)
	   | _ => (print ("error: " ^ Tcb.state_string state1 ^ "\n"); false)
       end

(*
---------------------------------------------------------------------
	22.	function test_estab_to_close_wait
*)

  fun test_estab_to_close_wait () =
       let val (state0, next_seq, next_ack) = make_estab ()
	   val fin_seg = Tcb.Seg {seq = next_seq, ack = next_ack,
				  len = SW.n32 "0", wnd = SW.n32 "1",
				  up = SW.n32 "0", options = [],
				  syn_flag = false, fin_flag = true,
				  reset_flag = false, ack_flag = true,
				  push_flag = true, urgent_flag = false}
	   val in_seg = Tcb.In_Seg {seg = fin_seg, data = B.Dyn_Array.empty}
           val state1 = Receive.receive (state0, in_seg)
	   val ack_seg = Tcb.Seg {seq = next_ack,
				  ack = FoxWord32.+ (next_seq, SW.n32 "1"),
				  len = SW.n32 "0", wnd = SW.n32 "4096",
				  up = SW.n32 "0", options = [],
				  syn_flag = false, fin_flag = false,
				  reset_flag = false, ack_flag = true,
				  push_flag = false, urgent_flag = false}
	   val out_ack = (Tcb.Out_Seg {seg = ack_seg,
				       data = B.Dyn_Array.empty}, NONE)
	   val expected_list = [Tcb.Send_Segment out_ack, Tcb.Peer_Close]
       in case state1 of
	     Tcb.Close_Wait (Tcb.Tcb {to_do, ...}) =>
	      check_to_do (! to_do, expected_list)
	   | _ => (print ("error: " ^ Tcb.state_string state1 ^ "\n"); false)
       end

(*
---------------------------------------------------------------------
	23.	function test_fw1_to_time_wait
*)

  fun test_fw1_to_time_wait () =
       let val (state0, next_seq, next_ack) = make_estab ()
           val state1 = State.close state0
	   val _ = case state1 of
	              Tcb.Fin_Wait_1 (Tcb.Tcb {to_do, ...}, _) => 
		       to_do := Tcb.Q.new ()
		    | _ => ()
	   val fa_seg = Tcb.Seg {seq = next_seq,
				 ack = FoxWord32.+ (next_ack, SW.n32 "1"),
				 len = SW.n32 "0", wnd = SW.n32 "4099",
				 up = SW.n32 "0", options = [],
				 syn_flag = false, fin_flag = true,
				 reset_flag = false, ack_flag = true,
				 push_flag = true, urgent_flag = false}
	   val in_seg = Tcb.In_Seg {seg = fa_seg, data = B.Dyn_Array.empty}
           val state2 = Receive.receive (state1, in_seg)
	   val ack_seg = Tcb.Seg {seq = FoxWord32.+ (next_ack, SW.n32 "1"),
				  ack = FoxWord32.+ (next_seq, SW.n32 "1"),
				  len = SW.n32 "0", wnd = SW.n32 "4096",
				  up = SW.n32 "0", options = [],
				  syn_flag = false, fin_flag = false,
				  reset_flag = false, ack_flag = true,
				  push_flag = false, urgent_flag = false}
	   val out_ack = (Tcb.Out_Seg {seg = ack_seg,
				       data = B.Dyn_Array.empty}, NONE)
	   val expected_list = [Tcb.Send_Segment out_ack,
				Tcb.Complete_Send (FoxWord32.+ (next_ack,
								SW.n32 "1")),
				Tcb.Complete_Close true,
				Tcb.Peer_Close]
       in case state2 of
	     Tcb.Time_Wait (Tcb.Tcb {to_do, ...}) =>
	      check_to_do (! to_do, expected_list)
	   | _ => (print ("error: " ^ Tcb.state_string state1 ^ "\n"); false)
       end

(*
---------------------------------------------------------------------
	24.	function test_fw1_to_fw2
*)

  fun test_fw1_to_fw2 () =
       let val (state0, next_seq, next_ack) = make_estab ()
           val state1 = State.close state0
	   val _ = case state1 of
	              Tcb.Fin_Wait_1 (tcb as (Tcb.Tcb {to_do, ...}, _)) => 
		       to_do := Tcb.Q.new ()
		    | _ => ()
	   val ack_seg = Tcb.Seg {seq = next_seq,
				  ack = FoxWord32.+ (next_ack, SW.n32 "1"),
				  len = SW.n32 "0", wnd = SW.n32 "0",
				  up = SW.n32 "88888", options = [],
				  syn_flag = false, fin_flag = false,
				  reset_flag = false, ack_flag = true,
				  push_flag = false, urgent_flag = false}
	   val in_seg = Tcb.In_Seg {seg = ack_seg, data = B.Dyn_Array.empty}
	   val state2 = Receive.receive (state1, in_seg)
       in case state2 of
	     Tcb.Fin_Wait_2 (Tcb.Tcb {to_do, ...}) =>
	      check_to_do (! to_do, [Tcb.Complete_Send (FoxWord32.+ (next_ack,
								 SW.n32 "1"))])
	   | _ => (print ("error: " ^ Tcb.state_string state2 ^ "\n"); false)
       end

(*
---------------------------------------------------------------------
	25.	function test_fw1_to_closing
*)

  fun test_fw1_to_closing () =
       let val (state0, next_seq, next_ack) = make_estab ()
           val state1 = State.close state0
	   val _ = case state1 of
	              Tcb.Fin_Wait_1 (Tcb.Tcb {to_do, ...}, _) => 
		       to_do := Tcb.Q.new ()
		    | _ => ()
	   val fin_seg = Tcb.Seg {seq = next_seq, ack = next_ack,
				  len = SW.n32 "0", wnd = SW.n32 "4099",
				  up = SW.n32 "0", options = [],
				  syn_flag = false, fin_flag = true,
				  reset_flag = false, ack_flag = true,
				  push_flag = true, urgent_flag = false}
	   val in_seg = Tcb.In_Seg {seg = fin_seg, data = B.Dyn_Array.empty}
           val state2 = Receive.receive (state1, in_seg)
	   val ack_seg = Tcb.Seg {seq = FoxWord32.+ (next_ack, SW.n32 "1"),
				  ack = FoxWord32.+ (next_seq, SW.n32 "1"),
				  len = SW.n32 "0", wnd = SW.n32 "4096",
				  up = SW.n32 "0", options = [],
				  syn_flag = false, fin_flag = false,
				  reset_flag = false, ack_flag = true,
				  push_flag = false, urgent_flag = false}
	   val out_ack = (Tcb.Out_Seg {seg = ack_seg,
				       data = B.Dyn_Array.empty}, NONE)
	   val expected_list = [Tcb.Send_Segment out_ack, Tcb.Peer_Close]
       in case state2 of
	     Tcb.Closing (Tcb.Tcb {to_do, ...}) =>
	      check_to_do (! to_do, expected_list)
	   | _ => (print ("error: " ^ Tcb.state_string state1 ^ "\n"); false)
       end

(*
---------------------------------------------------------------------
	26.	function test_close_wait_to_last_ack
*)

  fun test_close_wait_to_last_ack () =
       let val (state0, next_seq, next_ack) = make_estab ()
	   val fin_seg = Tcb.Seg {seq = next_seq, ack = next_ack,
				  len = SW.n32 "0", wnd = SW.n32 "1",
				  up = SW.n32 "0", options = [],
				  syn_flag = false, fin_flag = true,
				  reset_flag = false, ack_flag = true,
				  push_flag = true, urgent_flag = false}
	   val in_seg = Tcb.In_Seg {seg = fin_seg, data = B.Dyn_Array.empty}
           val state1 = Receive.receive (state0, in_seg)
	   val _ = case state1 of
	              Tcb.Close_Wait (Tcb.Tcb {to_do, ...}) =>
		       to_do := Tcb.Q.new ()
		    | _ => ()
           val state2 = State.close state1
	   val response = Tcb.Seg {seq = next_ack,
				   ack = FoxWord32.+ (next_seq, SW.n32 "1"),
				   len = SW.n32 "0", wnd = SW.n32 "4096",
				   up = SW.n32 "0", options = [],
				   syn_flag = false, fin_flag = true,
				   reset_flag = false, ack_flag = true,
				   push_flag = false, urgent_flag = false}
	   val out_fin = (Tcb.Out_Seg {seg = response,
				       data = B.Dyn_Array.empty}, NONE)
	   val expected_list = [Tcb.Send_Segment out_fin]
       in case state2 of
	     Tcb.Last_Ack (Tcb.Tcb {to_do, ...}) =>
	      check_to_do (! to_do, expected_list)
	   | _ => (print ("error: " ^ Tcb.state_string state1 ^ "\n"); false)
       end

(*
---------------------------------------------------------------------
	27.	function test_fw2_to_time_wait
*)

  fun test_fw2_to_time_wait () =
       let val (state0, next_seq, next_ack) = make_estab ()
           val state1 = State.close state0
	   val _ = case state1 of
	              Tcb.Fin_Wait_1 (Tcb.Tcb {to_do, ...}, _) => 
		       to_do := Tcb.Q.new ()
		    | _ => ()
	   val ack_seg = Tcb.Seg {seq = next_seq,
				  ack = FoxWord32.+ (next_ack, SW.n32 "1"),
				  len = SW.n32 "0", wnd = SW.n32 "123",
				  up = SW.n32 "0", options = [],
				  syn_flag = false, fin_flag = false,
				  reset_flag = false, ack_flag = true,
				  push_flag = true, urgent_flag = false}
	   val in_ack_seg = Tcb.In_Seg {seg = ack_seg,
					data = B.Dyn_Array.empty}
           val state2 = Receive.receive (state1, in_ack_seg)
	   val fin_seg = Tcb.Seg {seq = next_seq,
				  ack = FoxWord32.+ (next_ack, SW.n32 "1"),
				  len = SW.n32 "0", wnd = SW.n32 "16",
				  up = SW.n32 "0", options = [],
				  syn_flag = false, fin_flag = true,
				  reset_flag = false, ack_flag = true,
				  push_flag = false, urgent_flag = false}
	   val in_fin_seg = Tcb.In_Seg {seg = fin_seg,
					data = B.Dyn_Array.empty}
           val state3 = Receive.receive (state2, in_fin_seg)
	   val response = Tcb.Seg {seq = FoxWord32.+ (next_ack, SW.n32 "1"),
				   ack = FoxWord32.+ (next_seq, SW.n32 "1"),
				   len = SW.n32 "0", wnd = SW.n32 "4096",
				   up = SW.n32 "0", options = [],
				   syn_flag = false, fin_flag = false,
				   reset_flag = false, ack_flag = true,
				   push_flag = false, urgent_flag = false}
	   val out_ack = (Tcb.Out_Seg {seg = response,
				       data = B.Dyn_Array.empty}, NONE)
	   val expected_list = [Tcb.Send_Segment out_ack,
				Tcb.Complete_Send (FoxWord32.+ (next_ack,
								SW.n32 "1")),
				Tcb.Complete_Close true, Tcb.Peer_Close]
       in case state3 of
	     Tcb.Time_Wait (Tcb.Tcb {to_do, ...}) =>
	      check_to_do (! to_do, expected_list)
	   | _ => (print ("error: " ^ Tcb.state_string state1 ^ "\n"); false)
       end

(*
---------------------------------------------------------------------
	28.	function test_closing_to_time_wait
*)

  fun test_closing_to_time_wait () =
       let val (state0, next_seq, next_ack) = make_estab ()
           val state1 = State.close state0
	   val _ = case state1 of
	              Tcb.Fin_Wait_1 (Tcb.Tcb {to_do, ...}, _) => 
		       to_do := Tcb.Q.new ()
		    | _ => ()
	   val fin_seg = Tcb.Seg {seq = next_seq, ack = next_ack,
				  len = SW.n32 "0", wnd = SW.n32 "4099",
				  up = SW.n32 "0", options = [],
				  syn_flag = false, fin_flag = true,
				  reset_flag = false, ack_flag = true,
				  push_flag = true, urgent_flag = false}
	   val in_seg = Tcb.In_Seg {seg = fin_seg, data = B.Dyn_Array.empty}
           val state2 = Receive.receive (state1, in_seg)
	   val _ = case state2 of
	              Tcb.Closing (Tcb.Tcb {to_do, ...}) =>
		       to_do := Tcb.Q.new ()
		    | _ => ()
	   val ack_seg = Tcb.Seg {seq = FoxWord32.+ (next_seq, SW.n32 "1"),
				  ack = FoxWord32.+ (next_ack, SW.n32 "1"),
				  len = SW.n32 "0", wnd = SW.n32 "6",
				  up = SW.n32 "0", options = [],
				  syn_flag = false, fin_flag = false,
				  reset_flag = false,
				  ack_flag = true,
				  push_flag = true,
				  urgent_flag = true}
	   val in_ack = Tcb.In_Seg {seg = ack_seg, data = B.Dyn_Array.empty}
	   val state3 = Receive.receive (state2, in_ack)
	   val expected = [Tcb.Complete_Close true,
			   Tcb.Complete_Send (FoxWord32.+ (next_ack,
							   SW.n32 "1"))]
       in case state3 of
	     Tcb.Time_Wait (Tcb.Tcb {to_do, ...}) =>
	      check_to_do (! to_do, expected)
	   | _ => (print ("error: " ^ Tcb.state_string state1 ^ "\n"); false)
       end

(*
---------------------------------------------------------------------
	29.	function test_last_ack_to_closed
*)

  fun test_last_ack_to_closed () =
       let val (state0, next_seq, next_ack) = make_estab ()
	   val fin_seg = Tcb.Seg {seq = next_seq, ack = next_ack,
				  len = SW.n32 "0", wnd = SW.n32 "1",
				  up = SW.n32 "0", options = [],
				  syn_flag = false, fin_flag = true,
				  reset_flag = false, ack_flag = true,
				  push_flag = true, urgent_flag = false}
	   val in_seg = Tcb.In_Seg {seg = fin_seg, data = B.Dyn_Array.empty}
           val state1 = Receive.receive (state0, in_seg)
           val state2 = State.close state1
	   val _ = case state2 of
	              Tcb.Last_Ack (Tcb.Tcb {to_do, ...}) =>
		       to_do := Tcb.Q.new ()
		    | _ => local_print ("last_ack: wrong state " ^
					Tcb.state_string state2)
	   val ack_seg = Tcb.Seg {seq = FoxWord32.+ (next_seq, SW.n32 "1"),
				  ack = FoxWord32.+ (next_ack, SW.n32 "1"),
				  len = SW.n32 "0", wnd = SW.n32 "4",
				  up = SW.n32 "0", options = [],
				  syn_flag = false, fin_flag = false,
				  reset_flag = false, ack_flag = true,
				  push_flag = false, urgent_flag = false}
	   val in_ack = Tcb.In_Seg {seg = ack_seg, data = B.Dyn_Array.empty}
           val state3 = Receive.receive (state2, in_ack)
	   val actions = [Tcb.Complete_Close true, Tcb.Delete_Tcb]
       in case state3 of
	     Tcb.Closed (to_do, _) => check_to_do (! to_do, actions)
	   | _ => (print ("error: " ^ Tcb.state_string state3); false)
       end

(*
---------------------------------------------------------------------
	30.	function test_time_wait_to_closed
*)

  fun test_time_wait_to_closed () =
       let val (state0, next_seq, next_ack) = make_estab ()
           val state1 = State.close state0
	   val fa_seg = Tcb.Seg {seq = next_seq,
				 ack = FoxWord32.+ (next_ack, SW.n32 "1"),
				 len = SW.n32 "0", wnd = SW.n32 "4099",
				 up = SW.n32 "0", options = [],
				 syn_flag = false, fin_flag = true,
				 reset_flag = false, ack_flag = true,
				 push_flag = true, urgent_flag = false}
	   val in_seg = Tcb.In_Seg {seg = fa_seg, data = B.Dyn_Array.empty}
           val state2 = Receive.receive (state1, in_seg)
	   val _ = case state2 of
	              Tcb.Time_Wait (Tcb.Tcb {to_do, ...}) => 
		       to_do := Tcb.Q.new ()
		    | _ => ()
           val state3 = State.timeout (state2, Tcb.Time_Wait_Timer)
       in case state3 of
	     Tcb.Closed (to_do, _) => check_to_do (! to_do, [Tcb.Delete_Tcb])
	   | _ => (local_print ("error: " ^ Tcb.state_string state1); false)
       end

(*
---------------------------------------------------------------------
	31.	function run_tests
*)

  fun run_tests () =
       (B.Test.test ("closed -> listen", test_closed_to_listen);
	B.Test.test ("listen -> closed", test_listen_to_closed);
	B.Test.test ("closed -> syn-sent", test_closed_to_syn_sent);
	B.Test.test ("syn-sent -> closed", test_syn_sent_to_closed);
	B.Test.test ("listen -> syn_passive", test_listen_to_syn_passive);
	B.Test.test ("syn_passive -> listen", test_syn_passive_to_listen);
	B.Test.test ("syn_sent -> syn_active", test_syn_sent_to_syn_active);
	B.Test.test ("syn_passive -> estab", test_syn_passive_to_estab);
	B.Test.test ("syn_active -> estab", test_syn_active_to_estab);
	B.Test.test ("syn_sent -> estab", test_syn_sent_to_estab);
	B.Test.test ("syn_passive -> fin-wait-1", test_syn_passive_to_fw1);
	B.Test.test ("syn_active -> fin-wait-1", test_syn_active_to_fw1);
	B.Test.test ("estab -> fin-wait-1", test_estab_to_fw1);
	B.Test.test ("estab -> close-wait", test_estab_to_close_wait);
	B.Test.test ("fin-wait-1 -> time-wait", test_fw1_to_time_wait);
	B.Test.test ("fin-wait-1 -> fin-wait-2", test_fw1_to_fw2);
	B.Test.test ("fin-wait-1 -> closing", test_fw1_to_closing);
	B.Test.test ("close-wait -> last-ack", test_close_wait_to_last_ack);
	B.Test.test ("fin-wait-2 -> time-wait", test_fw2_to_time_wait);
	B.Test.test ("closing -> time-wait", test_closing_to_time_wait);
	B.Test.test ("last-ack -> closed", test_last_ack_to_closed);
	B.Test.test ("time-wait -> closed", test_time_wait_to_closed);
	())

(*
---------------------------------------------------------------------
	32.	function run
*)

  fun run () =
   B.Test.tests ("TcpState", 22, run_tests)

  val _ = if ! B.Debug.do_tests then run () else ()

 end (* struct *)

(*
		33.	structure Test_Tcp_State
*)

structure Test_Tcp_State = Test_Tcp_State (structure B = Fox_Basis
					   val debug_level = NONE)
