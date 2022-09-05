(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edo Biagioni (esb@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

	i.	Abstract

	tcpaction.tst: simple conformance-to-RFC test for the TCP actions.

---------------------------------------------------------------------

	ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor Test_Tcp_Action
	2.	function print_packet
	3.	function send
	4.	function allocate
	5.	function user_receive
	6.	function act
	7.	function make_tcb
	8.	function same_data
	9.	function same_segment
	10.	function same_action
	11.	function check_same_set
	12.	function check_to_do
	13.	function check_out
	14.	function build_in
	15.	function build_raw
	16.	function build_out
	17.	function test_send
	18.	function test_lower_receive
	19.	function test_set
	20.	function test_clear
	21.	function run_tests
	22.	function run
	23.	structure Test_Tcp_Action

---------------------------------------------------------------------
	iii.	RCS Log

$Log: tcpaction.tst,v $
Revision 1.23  1995/03/12  17:52:02  esb
adapted to new trace.sig.

Revision 1.22  1995/03/07  23:57:08  esb
updated tracing.

Revision 1.21  1995/02/21  13:05:02  esb
upgraded for SML/NJ 1.07.

Revision 1.20  1995/02/13  23:28:49  esb
adapted for 1.07.

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

Revision 1.13  1994/07/01  02:30:09  danwang
Moved control structures into Fox_Basis.

Revision 1.12  1994/06/16  16:42:04  danwang
Updated to use functorized Fox_Basis

Revision 1.11  1994/06/13  23:13:13  esb
moved timer implementation from tcpaction to tcptcb.

Revision 1.10  1994/05/10  08:07:17  esb
adapted to new tcpaction.sig

Revision 1.9  94/04/26  20:12:10  esb
changed timing constants.

Revision 1.8  94/04/26  17:59:29  esb
adapted to new COROUTINE and EVENT_QUEUE signatures.

Revision 1.7  94/04/06  23:15:38  esb
adapted to new receive_packet interface.

Revision 1.6  94/03/19  17:25:39  esb
added sending a packet of size two in test_send.

Revision 1.5  94/02/21  00:04:57  esb
adapted to new send_packet interface.

Revision 1.4  94/02/17  01:12:18  esb
interface changes.

Revision 1.3  94/02/14  14:25:04  esb
now ignores log messages.

Revision 1.2  94/01/19  21:27:06  esb
adapted to new interface.

Revision 1.1  1994/01/09  03:24:15  esb
Initial revision

*)

(*
---------------------------------------------------------------------
	1.	functor Test_Tcp_Action

	All page numbers refer to RFC 793 unless otherwise indicated.
 *)

functor Test_Tcp_Action (structure B: FOX_BASIS
			 val debug_level: int ref option): TEST_STRUCTURE =
 struct
  structure Trace = Trace (structure V = B.V
			   val debug_level = debug_level
			   val module_name = "tcpaction.tst")
  val local_print = Trace.local_print
  val trace_print = Trace.trace_print

  structure Log = Tcp_Log (structure B = B)

  structure Tcb = Tcp_Tcb (structure B = B
			   structure Tcp_Log = Log)

  fun print_exn name x = local_print (name ^ " exception " ^ System.exn_name x)

  structure Action = Tcp_Action (structure Tcp_Tcb = Tcb
				 val compute_checksums = true
				 val send_exception = print_exn "send"
				 val receive_exception = print_exn "receive"
				 structure B = B
				 val debug_level = debug_level)

(*
---------------------------------------------------------------------
	2.	function print_packet
*)

  fun print_packet (name, size, data) =
       let fun header B.Format.Hex = "x"
	     | header B.Format.Binary = "b"
	     | header B.Format.Decimal = ""
	   val _ = B.Format.byte_header := header
	   val format = [B.Format.String (name ^ " packet header: lp "),
			 B.Format.Bytes 2,
			 B.Format.String ", rp ",
			 B.Format.Bytes 2,
			 B.Format.String ", seq ",
			 B.Format.Bytes 4,
			 B.Format.String ", ack ",
			 B.Format.Bytes 4,
			 B.Format.String ", len*16 ",
			 B.Format.Bytes 1,
			 B.Format.String ", flags ",
			 B.Format.Bytes 1,
			 B.Format.String ", window ",
			 B.Format.Bytes 2,
			 B.Format.String ", checksum ",
			 B.Format.Bytes 2,
			 B.Format.String ", up ",
			 B.Format.Bytes 2,
			 B.Format.String "\nfirst n bytes: ",
			 B.Format.Bytes (min (20, size - 20))]
	   val strings = B.Format.bytearray (format, data, 0)
	   val string = fold (op ^) strings ""
       in local_print string
       end

(*
---------------------------------------------------------------------
	3.	function send
*)

  fun send (record, packet) =
       (if Trace.trace_on () then
	  print_packet ("outgoing", B.Dyn_Array.size packet,
			B.Dyn_Array.read packet)
	else ();
	record := (! record) @ [packet])

(*
---------------------------------------------------------------------
	4.	function allocate
*)

  fun allocate record size =
       let val packet = B.Dyn_Array.new size
	   fun send_fun () = send (record, packet)
       in (packet, send_fun)
       end

(*
---------------------------------------------------------------------
	5.	function user_receive
*)

  fun user_receive record packet =
       (if Trace.trace_on () then
	 print_packet ("user", B.Dyn_Array.size packet,
		       B.Dyn_Array.read packet)
	else ();
	record := (! record) @ [packet])

(*
---------------------------------------------------------------------
	6.	function act
*)

  fun act record () =
       (trace_print (fn _ => "action procedure called");
	record := () :: (! record))

(*
---------------------------------------------------------------------
	7.	function make_tcb
*)

  val incarnation = ref 1

  fun make_tcb (to_do, id) =
       let val ref4 = ref (SW.n32 "0")
	   val ref2 = ref (SW.n16 "0")
	   val refi = ref 0
	   val refb = ref false
	   val current_call = ! incarnation
	   val _ = incarnation := current_call + 1
	   fun print_action (action, string) =
	        Tcb.action_string action ^ " " ^ string
	   fun exec timer =
	        (to_do := Tcb.Q.add (! to_do, Tcb.Timer_Expiration timer);
		 trace_print (fn _ => "timer expiration for " ^ id ^
			      ", incarnation " ^
			      makestring current_call ^
			      ", to-do list is [" ^
			      (Tcb.Q.fold print_action (! to_do) "]")))
       in Tcb.Tcb {iss = (SW.n32 "0"), snd_una = ref4, snd_nxt = ref4,
		   snd_wnd = ref4, max_snd_wnd = ref4, snd_wl1 = ref4,
		   snd_wl2 = ref4, snd_wnd_scale = ref 0,
		   irs = ref4, rcv_nxt = ref4,
		   rcv_wnd = ref4, rcv_sws = ref4,
		   rcv_wnd_scale = ref 0,
		   wanted_wnd_scale = FoxWord8.intToWord 0,
		   send_immediately = refb,
		   cwnd = refi, ssthresh = refi, unacked_segs = refi,
		   srtt = refi, srtd = refi, srto = refi, wto = refi,
		   mss = ref2, resend = ref (Tcb.D.new ()),
		   queued = ref (Tcb.D.new ()),
		   out_of_order = ref (Tcb.Q.new ()),
		   timers = Tcb.empty_timer_set exec, to_do = to_do}
       end

(*
---------------------------------------------------------------------
	8.	function same_data
*)

  fun same_data (data1, s1, data2, s2) =
       let fun same_bytes (b1, off1, l1, b2, off2, l2) =
	        if off1 = l1 andalso off2 = l2 then true
	        else if off1 >= l1 orelse off2 >= l2 then false
	        else if ByteArray.sub (b1, off1) <>
		        ByteArray.sub (b2, off2) then false
	        else same_bytes (b1, off1 + 1, l1, b2, off2 + 1, l2)
	   val t1 = s1 = s2
	   val t2 = same_bytes (data1, 0, s1, data2, 0, s2)
	   val result = t1 andalso t2
	   val bytes_string =
	        if t2 then ""
		else (", bytes are " ^
		      FoxMakestring.word8 (FoxWord8.sub (data1, 0)) ^ "." ^
		      FoxMakestring.word8 (FoxWord8.sub (data1, 1)) ^ ", " ^
		      FoxMakestring.word8 (FoxWord8.sub (data2, 0)) ^ "." ^
		      FoxMakestring.word8 (FoxWord8.sub (data2, 1)))
	   val res_string =
	    "(" ^ FoxMakestring.bool t1 ^ " & " ^ FoxMakestring.bool t2 ^ ")"
	   val num_true = (if t1 then 1 else 0) + (if t2 then 1 else 0)
       in if result then true
	  else
	   (if num_true > 0 orelse
	       (Trace.trace_on () andalso num_true >= 0) then
	     local_print ("size1 = " ^ makestring s1 ^ ", " ^
			  "size2 = " ^ makestring s2 ^ bytes_string ^ "\n" ^
			  res_string)
	    else ();
	    false)
       end

(*
---------------------------------------------------------------------
	9.	function same_segment
*)

  fun same_segment (seg1, seg2) =
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
	   val t1 = seq1 = seq2
	   val t2 = not acf1 orelse ack1 = ack2
	   val t3 = len1 = len2
	   val t4 = wnd1 = wnd2
	   val t5 = not urg1 orelse up1 = up2
	   val t6 = syn1 = syn2 andalso fin1 = fin2 andalso
		    acf1 = acf2 andalso rst1 = rst2 andalso
		    urg1 = urg2 andalso psh1 = psh2
	   val result = t1 andalso t2 andalso t3 andalso t4 andalso
	                t5 andalso t6
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
		  FoxMakestring.bool t6 ^ ")"
       in if result then true
	  else
	   (local_print
	       ("seg1 = " ^ Tcb.segment_string seg1 ^ ", " ^
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
		res_string);
            false)
       end (* let *)
(*
---------------------------------------------------------------------
	10.	function same_action
*)

  fun same_action (Tcb.User_Error s1, Tcb.User_Error s2) = s1 = s2
    | same_action (Tcb.Send_Segment (s1, _), Tcb.Send_Segment (s2, _)) =
       let fun rebuild_seg (seq, ack, len, wnd, push) =
	        Tcb.Seg {seq = seq, ack = ack, len = len, wnd = wnd,
			 up = (SW.n32 "0"), options = [], syn_flag = false,
			 fin_flag = false, reset_flag = false,
			 ack_flag = true, push_flag = push,
			 urgent_flag = false}
	   val (seg1, data1) =
	        case s1 of
		   Tcb.Out_Seg {seg, data} => (seg, data)
		 | Tcb.Fast_Out {seq, ack, len, wnd, data} =>
		    (rebuild_seg (seq, ack, len, wnd, true), data)
		 | Tcb.Fast_Empty {seq, ack, wnd} =>
		    (rebuild_seg (seq, ack, (SW.n32 "0"), wnd, false)
		     , B.Dyn_Array.new 0)
	   val (seg2, data2) =
	        case s2 of
		   Tcb.Out_Seg {seg, data} => (seg, data)
		 | Tcb.Fast_Out {seq, ack, len, wnd, data} =>
		    (rebuild_seg (seq, ack, len, wnd, true), data)
		 | Tcb.Fast_Empty {seq, ack, wnd} =>
		    (rebuild_seg (seq, ack, (SW.n32 "0"), wnd, true),
		     B.Dyn_Array.new 0)
	   val s1 = B.Dyn_Array.size data1
	   val s2 = B.Dyn_Array.size data2
	   val d1 = B.Dyn_Array.read data1
	   val d2 = B.Dyn_Array.read data2
       in if same_data (d1, s1, d2, s2) then
	   same_segment (seg1, seg2)
	  else
	   (local_print ("size1 = " ^ makestring (B.Dyn_Array.size data1) ^
			 ", " ^
			 "size2 = " ^ makestring (B.Dyn_Array.size data2));
            false)
       end (* let *)
    | same_action (Tcb.User_Data packet1, Tcb.User_Data packet2) =
       let val s1 = B.Dyn_Array.size packet1
           val s2 = B.Dyn_Array.size packet2
	   val d1 = B.Dyn_Array.read packet1
           val d2 = B.Dyn_Array.read packet2
       in same_data (d1, s1, d2, s2)
       end
    | same_action (Tcb.Process_Data (Tcb.Fast_In {seq, ack, len, wnd, data}),
		   x) =
       let val seg = Tcb.Seg {seq = seq, ack = ack, len = len, wnd = wnd,
			      up = (SW.n32 "0"), options = [],
			      syn_flag = false, fin_flag = false,
			      ack_flag = true, push_flag = true,
			      reset_flag = false, urgent_flag = false}
	   val input = Tcb.In_Seg {seg = seg, data = data}
       in same_action (Tcb.Process_Data input, x)
       end
    | same_action (x, (y as Tcb.Process_Data (Tcb.Fast_In {seq, ...}))) =
       same_action (y, x)
    | same_action (Tcb.Process_Data (Tcb.In_Seg {seg = seg1, data = data1}),
		   Tcb.Process_Data (Tcb.In_Seg {seg = seg2, data = data2})) =
       let val s1 = B.Dyn_Array.size data1
           val s2 = B.Dyn_Array.size data2
	   val d1 = B.Dyn_Array.read data1
           val d2 = B.Dyn_Array.read data2
       in if same_data (d1, s1, d2, s2) then
	   same_segment (seg1, seg2)
	  else
	   (local_print
	       ("size1 = " ^ makestring (B.Dyn_Array.size data1) ^ ", " ^
		"size2 = " ^ makestring (B.Dyn_Array.size data2) ^ "\n");
            false)
       end (* let *)
    | same_action (Tcb.Timer_Expiration Tcb.Resend_Timer,
		   Tcb.Timer_Expiration Tcb.Resend_Timer) = true
    | same_action (Tcb.Timer_Expiration Tcb.User_Timer,
		   Tcb.Timer_Expiration Tcb.User_Timer) = true
    | same_action (Tcb.Timer_Expiration Tcb.Time_Wait_Timer,
		   Tcb.Timer_Expiration Tcb.Time_Wait_Timer) = true
    | same_action (Tcb.Timer_Expiration Tcb.Ack_Timer,
		   Tcb.Timer_Expiration Tcb.Ack_Timer) = true
    | same_action (Tcb.Timer_Expiration Tcb.Window_Timer,
		   Tcb.Timer_Expiration Tcb.Window_Timer) = true
    | same_action (Tcb.Close_After_Sends, Tcb.Close_After_Sends) = true
    | same_action (Tcb.Complete_Open x, Tcb.Complete_Open y) = x = y
    | same_action (Tcb.Complete_Close x, Tcb.Complete_Close y) = x = y
    | same_action (Tcb.Probe_Window, Tcb.Probe_Window) = true
    | same_action (Tcb.Delete_Tcb, Tcb.Delete_Tcb) = true
    | same_action (Tcb.Log_Event _, Tcb.Log_Event _) = true
    | same_action (Tcb.Peer_Close, Tcb.Peer_Close) = true
    | same_action (Tcb.Peer_Reset, Tcb.Peer_Reset) = true
    | same_action (_, _) = false

(*
---------------------------------------------------------------------
	11.	function check_same_set
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
	12.	function check_to_do
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
	13.	function check_out
*)

  fun check_out ([], []) = true
    | check_out ([], _) =
       (local_print "check_out failed: actual list too short";
        false)
    | check_out (_, []) =
       (local_print "check_out failed: actual list too long";
        false)
    | check_out (actual_d :: actual_rest, exp_head :: exp_rest) =
       let fun rebuild_seg (seq, ack, len, wnd, push) =
	        Tcb.Seg {seq = seq, ack = ack, len = len, wnd = wnd,
			 up = (SW.n32 "0"), options = [], syn_flag = false,
			 fin_flag = false, reset_flag = false,
			 ack_flag = true, push_flag = push,
			 urgent_flag = false}
	   val (exp_seg, exp_d) =
	        case exp_head of
		   Tcb.Out_Seg {seg, data} => (seg, data)
		 | Tcb.Fast_Out {seq, ack, len, wnd, data} =>
		    (rebuild_seg (seq, ack, len, wnd, true), data)
		 | Tcb.Fast_Empty {seq, ack, wnd} =>
		    (rebuild_seg (seq, ack, (SW.n32 "0"), wnd, false),
		     B.Dyn_Array.new 0)
	   val packet_data = B.Dyn_Array.read actual_d
	   val ntohl = B.Order.B4.from_big
	   val ntohs = B.Order.B2.from_big
	   val seq = ntohl (FoxWord32.sub (packet_data, 4))
	   val ack = ntohl (FoxWord32.sub (packet_data, 8))
	   val len = B.Dyn_Array.size actual_d
	   val wnd = FoxWord32.intToWord (FoxWord16.wordToInt
				       (ntohs (FoxWord16.sub (packet_data, 14))))
	   val up = FoxWord32.intToWord (FoxWord16.wordToInt
				       (ntohs (FoxWord16.sub (packet_data, 18))))
	   val flags = FoxWord8.sub (packet_data, 13)
	   val is_ack = FoxWord8.andb (flags, SW.n8 "0x10") <> SW.n8 "0"
	   val is_psh = FoxWord8.andb (flags, SW.n8 "0x08") <> SW.n8 "0"
	   val is_urg = FoxWord8.andb (flags, SW.n8 "0x20") <> SW.n8 "0"
	   val actual_seg = Tcb.Seg {seq = seq, ack = ack,
				     len = FoxWord32.intToWord (len - 20),
				     wnd = wnd, up = up, options = [],
				     syn_flag = false, fin_flag = false,
				     reset_flag = false, ack_flag = is_ack,
				     push_flag = is_psh, urgent_flag = is_urg}
	   val actual_size = len - 20
	   val actual_data = B.Dyn_Array.sub (actual_d, 20, actual_size)
	   val exp_size = B.Dyn_Array.size exp_d
	   val exp_data = B.Dyn_Array.read exp_d
       in if same_segment (actual_seg, exp_seg) then
	   if same_data (actual_data, actual_size, exp_data, exp_size) then
	    check_out (actual_rest, exp_rest)
	   else
	    (local_print "check_out failed: different data";
	     false)
	  else
	   (local_print "check_out failed: different headers";
	    false)
       end

(*
---------------------------------------------------------------------
	14.	function build_in
*)

  fun build_in (seq, ack, len, wnd, push, urgent) =
       let val seg = Tcb.Seg {seq = FoxWord32.intToWord seq,
			      ack = FoxWord32.intToWord ack,
			      len = FoxWord32.intToWord len,
			      wnd = FoxWord32.intToWord wnd,
			      up = (SW.n32 "0"),
			      options = [],
			      syn_flag = false,
			      fin_flag = false,
			      reset_flag = false,
			      ack_flag = true,
			      push_flag = push,
			      urgent_flag = urgent}
	   fun build_segment (len, offset) =
	        let fun fill n = FoxWord8.intToWord ((n + offset) mod 256)
		in B.Dyn_Array.init1 (len, fill)
		end
	   val in_data = build_segment (len, seq)
	   val in_seg = Tcb.In_Seg {seg = seg, data = in_data}
       in in_seg
       end

(*
---------------------------------------------------------------------
	15.	function build_raw
*)

  fun build_raw (lp, rp, seq, ack, len, wnd, push, urgent, peer_cksum) =
       let fun fill (n, _) = FoxWord8.intToWord ((n + seq - 20) mod 256)
	   fun b2hton (n, b, offset) =
	        B.Dyn_Array.update2 (b, offset, B.Order.B2.to_big n);
	   fun b4hton (n, b, offset) =
	        B.Dyn_Array.update4 (b, offset, B.Order.B4.to_big n);
	   val tlen = len + 20
	   val data = B.Dyn_Array.new tlen
	   val _ = B.Dyn_Array.app1 (data, fill)
	   val flags = case (push, urgent) of
	                  (true, true) => SW.n8 "0x38"
			| (true, false) => SW.n8 "0x18"
			| (false, true) => SW.n8 "0x30"
			| (false, false) => SW.n8 "0x10"
	   fun check () =
	        let val raw = B.Dyn_Array.checksum (data, 0, tlen)
		    val add = B.Checksum.one_s_add 
		    val b2len = FoxWord16.intToWord tlen
		    val total = add (raw, add (peer_cksum, b2len))
		in B.Checksum.one_s_complement total
		end
       in b2hton (FoxWord16.intToWord lp, data, 0);
	  b2hton (FoxWord16.intToWord rp, data, 2);
	  b4hton (FoxWord32.intToWord seq, data, 4);
	  b4hton (FoxWord32.intToWord ack, data, 8);
	  B.Dyn_Array.update1 (data, 12, SW.n8 "0x50");
	  B.Dyn_Array.update1 (data, 13, flags);
	  b2hton (FoxWord16.intToWord wnd, data, 14);
	  b2hton (SW.n16 "0", data, 16);
	  b2hton (SW.n16 "0", data, 18);
	  b2hton (check (), data, 16);
	  data
       end

(*
---------------------------------------------------------------------
	16.	function build_out
*)

  fun build_out (seq, ack, len, wnd, push, urgent) =
       let fun build_segment (len, offset) =
	        let fun fill n = FoxWord8.intToWord ((n + offset) mod 256)
		in B.Dyn_Array.init1 (len, fill)
		end
	   val out_data = build_segment (len, seq)
       in case (push, urgent, len) of
	     (false, false, 0) =>
	      Tcb.Fast_Empty {seq = FoxWord32.intToWord seq,
			      ack = FoxWord32.intToWord ack,
			      wnd = FoxWord32.intToWord wnd}
	   | (true, false, _) =>
	      Tcb.Fast_Out {seq = FoxWord32.intToWord seq,
			    ack = FoxWord32.intToWord ack,
			    len = FoxWord32.intToWord len,
			    wnd = FoxWord32.intToWord wnd,
			    data = out_data}
	   | (_, _, _) =>
	      Tcb.Out_Seg {seg = Tcb.Seg {seq = FoxWord32.intToWord seq,
					  ack = FoxWord32.intToWord ack,
					  len = FoxWord32.intToWord len,
					  wnd = FoxWord32.intToWord wnd,
					  up = (SW.n32 "0"),
					  options = [],
					  syn_flag = false,
					  fin_flag = false,
					  reset_flag = false,
					  ack_flag = true,
					  push_flag = push,
					  urgent_flag = urgent},
	                   data = out_data}
       end

(*
---------------------------------------------------------------------
	17.	function test_send
*)

  fun test_send () =
       let val sent = ref ([]: B.Dyn_Array.T list)
	   val acted = ref ([]: unit list)
	   val to_do = ref (Tcb.Q.new (): Tcb.tcp_action Tcb.Q.T)
	   val address = Action.Address {local_port = SW.n16 "222",
					 remote_port = SW.n16 "444",
					 peer_checksum = SW.n16 "0"}
	   val state = Action.new {allocate = allocate sent, act = act acted,
				   to_do = to_do, address = address}
	   val p1 = build_out (999, 333, 1460, 1024, true, false)
	   val p2 = build_out (999, 333, 0, 1024, false, false)
	   val p3 = build_out (999, 333, 2, 1024, true, true)
       in Action.send_segment (state, p1);
	  Action.send_segment (state, p2);
	  Action.send_segment (state, p3);
	  (check_to_do (! to_do, []) andalso
	   check_out (! sent, [p1, p2, p3]))
       end

(*
---------------------------------------------------------------------
	18.	function test_lower_receive
*)

  fun test_lower_receive () =
       let val sent = ref ([]: B.Dyn_Array.T list)
	   val acted = ref ([]: unit list)
	   val to_do = ref (Tcb.Q.new (): Tcb.tcp_action Tcb.Q.T)
	   val address = Action.Address {local_port = SW.n16 "222",
					 remote_port = SW.n16 "444",
					 peer_checksum = SW.n16 "0"}
	   val state = Action.new {allocate = allocate sent, act = act acted,
				   to_do = to_do, address = address}
	   val p = build_raw (222, 444, 999, 333, 1460, 1024,
			      true, false, SW.n16 "0")
	   val min_header = B.Dyn_Array.sub (p, 0, 20)
	   val seg = build_in (999, 333, 1460, 1024, true, false)
	   val pseudo_check = SW.n16 "1480"
       in Action.process_packet (state, p, pseudo_check, min_header);
	  check_to_do (! to_do, [Tcb.Process_Data seg])
       end

(*
---------------------------------------------------------------------
	19.	function test_set
*)

  fun test_set (name, timer, time) () =
       let val to_do = ref (Tcb.Q.new (): Tcb.tcp_action Tcb.Q.T)
	   val tcb = make_tcb (to_do, "set_" ^ name)
	   val Tcb.Tcb {timers, ...} = tcb
       in Tcb.start_timer (timers, timer, time);
	  if not (Tcb.Q.empty (! to_do)) then
	   (local_print ("error: set " ^ name ^ " to_do list has size " ^
			 B.V.Integer.makestring (Tcb.Q.size (! to_do)));
	    false)
	  else
	   (B.Scheduler.sleep (time * 2 + 500);
	    if Tcb.Q.empty (! to_do) then
	     (local_print ("error: " ^ name ^ " timer did not expire");
	      false)
	    else
	     check_to_do (! to_do, [Tcb.Timer_Expiration timer]))
       end

(*
---------------------------------------------------------------------
	20.	function test_clear
*)

  fun test_clear (name, timer, time) () =
       let val to_do = ref (Tcb.Q.new (): Tcb.tcp_action Tcb.Q.T)
	   val tcb = make_tcb (to_do, "clear_" ^ name)
	   val Tcb.Tcb {timers, ...} = tcb
       in Tcb.start_timer (timers, timer, time);
	  if not (Tcb.Q.empty (! to_do)) then
	   (local_print ("error: clear " ^ name ^ " expired after setting");
	    check_to_do (! to_do, []))
	  else
	   (B.Scheduler.sleep (time div 3);
	    if not (Tcb.Q.empty (! to_do)) then
	     (local_print ("error: clear " ^ name ^ " expired early");
	      check_to_do (! to_do, []))
	    else
	     (Tcb.stop_timer (timers, timer);
	      B.Scheduler.sleep time;
	      if not (Tcb.Q.empty (! to_do)) then
	       (local_print ("error: clear " ^ name ^
			     " expired after clearing");
		check_to_do (! to_do, []))
	      else true))
       end

(*
---------------------------------------------------------------------
	21.	function run_tests
*)

  fun run_tests () =
       (B.Scheduler.reset ();
        B.Test.test ("send", test_send);
        B.Test.test ("lower receive", test_lower_receive);
	(* if you remove this sleep statement, the call to set
	   retransmit timer fails (the timer never expires) even
	   with very long times.  With the sleep statement, the
	   test seems reliable. Might be worth investigating,
	   but not right now.  -- esb. *)
	B.Scheduler.sleep 100;
        B.Test.test ("set retransmit timer",
		     test_set ("retransmit", Tcb.Resend_Timer, 100));
        B.Test.test ("set wait timer",
		     test_set ("wait", Tcb.Time_Wait_Timer, 300));
        B.Test.test ("set user timer", test_set ("user", Tcb.User_Timer, 30));
        B.Test.test ("set ack timer", test_set ("ack", Tcb.Ack_Timer, 1000));
        B.Test.test ("set window timer",
		     test_set ("window", Tcb.Ack_Timer, 3000));
        B.Test.test ("clear retransmit timer",
		     test_clear ("retransmit", Tcb.Resend_Timer, 1500));
        B.Test.test ("clear wait timer",
		     test_clear ("wait", Tcb.Time_Wait_Timer, 1100));
        B.Test.test ("clear user timer",
		     test_clear ("user", Tcb.User_Timer, 1000));
        B.Test.test ("clear ack timer",
		     test_clear ("ack", Tcb.Ack_Timer, 1300));
        B.Test.test ("clear window timer",
		     test_clear ("window", Tcb.Window_Timer, 400));
        ())

(*
---------------------------------------------------------------------
	22.	function run
*)

  fun run () =
       B.Test.tests ("TcpAction", 12, run_tests)

  val _ = if ! B.Debug.do_tests then run () else ()

 end (* struct *)

(*
		23.	structure Test_Tcp_Action
*)

structure Test_Tcp_Action = Test_Tcp_Action (structure B = Fox_Basis
					     val debug_level = NONE)








