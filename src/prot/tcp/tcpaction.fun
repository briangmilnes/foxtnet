(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edo Biagioni (esb@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

	i.	Abstract

	tcpaction.fun: the implementation of the actions specified
	by the TCP finite state machine. The actions are those
	defined in tcptcb.sig.

---------------------------------------------------------------------

	ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor Tcp_Action
	2.	internal TCP flag bits
	3.	internal TCP header definition
	4.	internal debugging function print_send
	5.	internal function add_header
	6.	function send_segment
	7.	function send_preallocated
	8.	function allocate_segment
	9.	internal function create_header
	10.	function process_packet
	11.	function new

---------------------------------------------------------------------

	iii.	RCS Log

$Log: tcpaction.fun,v $
Revision 1.27  1995/03/10  03:47:37  esb
adapted to new vendor.sig.

Revision 1.26  1995/03/07  23:57:08  esb
updated tracing.

Revision 1.25  1995/02/22  15:28:03  esb
minor changes.

Revision 1.24  1995/02/04  20:40:26  robby
updated to 107

Revision 1.23  1995/01/14  02:28:30  esb
./tcpstateadded tcp window-scale option.

Revision 1.22  1994/10/11  15:43:04  cline
Upgraded to work with SML/NJ 1.05b

Revision 1.21  1994/08/24  22:10:48  esb
added minor optimizations and streamlining.

Revision 1.20  1994/08/18  20:31:09  esb
adapted to new tcptcb.sig, with status message support.

Revision 1.19  1994/08/17  16:28:26  esb
minor tuning and bug fixes.

Revision 1.18  1994/08/12  06:27:15  esb
converted to the new protocol signature.

Revision 1.17  1994/07/11  17:52:03  esb
optimized to produce Fast_In messages when possible.

Revision 1.16  1994/07/04  21:33:46  esb
adapted to Copy/Create split.

Revision 1.15  1994/07/01  02:30:02  danwang
Moved control structures into Fox_Basis.

Revision 1.14  1994/06/16  16:42:04  danwang
Updated to use functorized Fox_Basis

Revision 1.13  1994/06/13  23:13:13  esb
moved timer implementation from tcpaction to tcptcb.

Revision 1.12  1994/05/10  08:05:19  esb
removed the receive function, optimized.

Revision 1.11  94/04/26  17:59:29  esb
adapted to new COROUTINE and EVENT_QUEUE signatures.

Revision 1.10  94/04/06  23:15:38  esb
adapted to new receive_packet interface.

Revision 1.9  94/03/19  17:24:07  esb
minor renaming.

Revision 1.8  94/03/10  19:43:49  esb
introduced copy.create, changed messages.

Revision 1.7  94/02/28  14:21:57  esb
added timing board calls.

Revision 1.6  94/02/21  00:08:03  esb
removed unnecessary handlers and adapted send to new send_packet interface.

Revision 1.5  94/02/17  01:09:59  esb
interface changes.

Revision 1.4  94/02/14  14:23:57  esb
adapted to new interface.

Revision 1.3  1994/01/28  01:19:26  esb
minor changes.

Revision 1.2  1994/01/19  21:27:06  esb
adapted to new interface.

Revision 1.1  1994/01/09  03:24:15  esb
Initial revision


---------------------------------------------------------------------
	1.	functor Tcp_Action
*)

functor Tcp_Action (structure Tcp_Tcb: TCP_TCB
		    structure B: FOX_BASIS
		    sharing type Tcp_Tcb.incoming_data = B.Dyn_Array.T
			and type Tcp_Tcb.outgoing_data = B.Dyn_Array.T
		    val compute_checksums: bool
		    val send_exception: exn -> unit
		    val debug_level: int ref option): TCP_ACTION =
 struct
  structure Trace = Trace (structure V = B.V
			   val debug_level = debug_level
			   val module_name = "tcpaction.fun")
  val local_print = Trace.local_print

  type segment = Tcp_Tcb.tcp_out
  type send_packet = B.Dyn_Array.T
  type receive_packet = B.Dyn_Array.T
  type user_packet = B.Dyn_Array.T
  type to_do_list = Tcp_Tcb.tcp_action Tcp_Tcb.Q.T ref
  datatype address = Address of {local_port: FoxWord16.word,
				 remote_port: FoxWord16.word,
				 peer_checksum: FoxWord16.word}

  structure Timer = Timer (structure Scheduler = B.Scheduler)

  datatype action_state =
        T of {allocate: int -> (send_packet * (unit -> unit)),
	      act: (unit -> unit),
	      local_port: FoxWord16.word,
	      remote_port: FoxWord16.word,
	      peer_checksum: FoxWord16.word,
	      header_template: ByteArray.bytearray,
	      empty_packet: send_packet,
	      empty_checksum: FoxWord16.word,
	      empty_send: unit -> unit,
	      to_do: to_do_list}

(*
---------------------------------------------------------------------
	2.	internal TCP flag bits
 *)

  val no_flag  = SW.n8 "0x00"
  val fin_flag = SW.n8 "0x01"
  val syn_flag = SW.n8 "0x02"
  val rst_flag = SW.n8 "0x04"
  val psh_flag = SW.n8 "0x08"
  val ack_flag = SW.n8 "0x10"
  val urg_flag = SW.n8 "0x20"

(*
---------------------------------------------------------------------
	3.	internal TCP header definition
 *)

  val min_hlen = 20
  val src_index = 0
  val dest_index = 2
  val seq_index = 4
  val ack_index = 8
  val hlen_index = 12
  val flag_index = 13
  val window_index = 14
  val cks_index = 16
  val urg_index = 18

(* max_hlen is used for sending the max-segment-size option in any packet
   that carries a SYN flag. Our max segment size is the network's MTU.
   The opt_*_offset values are with respect to the beginning of
   each option. Maximum Segment Size is the only non-trivial option. *)
  val max_hlen = min_hlen + 4
  val opt_kind_offset = 0
  val opt_length_offset = 1
  val opt_value_offset = 2
  val eoo_option_code = 0		(* end-of-option *)
  val eoo_option_length = 1
  val nop_option_code = 1		(* no-op *)
  val nop_option_length = 1
  val mss_option_code = 2
  val mss_option_length = 4
  val window_scale_option_code = 3
  val window_scale_option_length = 3

(*
---------------------------------------------------------------------
	4.	internal debugging function print_send
*)

  fun print_send packet = local_print ("send " ^ Tcp_Tcb.out_string packet)

(*
---------------------------------------------------------------------
	5.	internal function add_header

	Make_header generates a header for the specified packet.  The
	checksum is the correct checksum for the packet.
*)

  val zero_check = SW.n16 "0"
  val ffff_check = SW.n16 "0xffff"

  local
   fun header_len [] = min_hlen
     | header_len ((Tcp_Tcb.Max_Segment {size}) :: rest) =
        mss_option_length + header_len rest
     | header_len ((Tcp_Tcb.Window_Scale {shift}) :: rest) =
        window_scale_option_length + header_len rest
(* redundant match, may be required if Tcp_Tcb declares more TCP options.
     | header_len (_ :: rest) = header_len rest
*)

   val htons = B.Order.B2.to_big
   val htonl = B.Order.B4.to_big

   fun write_options (packet, offset, hlen, []) =
        if offset < hlen then
         (B.Dyn_Array.update1 (packet, offset,
			       FoxWord8.intToWord eoo_option_code);
	  write_options (packet, offset + eoo_option_length, hlen, []))
	else ()
     | write_options (packet, offset, hlen,
		      Tcp_Tcb.Max_Segment {size} :: rest) =
        (B.Dyn_Array.update1 (packet, offset,
			      FoxWord8.intToWord mss_option_code);
	 B.Dyn_Array.update1 (packet, offset + opt_length_offset,
			      FoxWord8.intToWord mss_option_length);
         B.Dyn_Array.update2 (packet, offset + opt_value_offset, htons size);
	 write_options (packet, offset + mss_option_length, hlen, rest))
     | write_options (packet, offset, hlen,
		      Tcp_Tcb.Window_Scale {shift} :: rest) =
        (B.Dyn_Array.update1 (packet, offset,
			      FoxWord8.intToWord window_scale_option_code);
	 B.Dyn_Array.update1 (packet, offset + opt_length_offset,
			      FoxWord8.intToWord window_scale_option_length);
         B.Dyn_Array.update1 (packet, offset + opt_value_offset, shift);
	 write_options (packet, offset + window_scale_option_length,
			hlen, rest))
(* redundant match, may be required if Tcp_Tcb declares more TCP options.
     | write_options (packet, offset, hlen, _ :: rest) =
        write_options (packet, offset, hlen, rest)
*)

   fun make_syn true = syn_flag | make_syn false = no_flag
   fun make_fin true = fin_flag | make_fin false = no_flag
   fun make_rst true = rst_flag | make_rst false = no_flag
   fun make_ack true = ack_flag | make_ack false = no_flag
   fun make_psh true = psh_flag | make_psh false = no_flag
   fun make_urg true = urg_flag | make_urg false = no_flag

   val ack_push = FoxWord8.orb (ack_flag, psh_flag)

   fun make_flags (false, false, false, true, true, false) = ack_push
     | make_flags (false, false, false, true, false, false) = ack_flag
     | make_flags (s, f, r, a, p, u) =
        FoxWord8.orb (FoxWord8.orb (FoxWord8.orb (make_syn s, make_fin f),
				    FoxWord8.orb (make_rst r, make_ack a)),
		      FoxWord8.orb (make_psh p, make_urg u))

   val add = B.Checksum.one_s_add
   val neg = B.Checksum.one_s_complement

   val ffff = SW.n32 "0xffff"

   fun do_check_new (seq, ack, window, rest) =
        let val seq_high = FoxWord16.intToWord
	                    (FoxWord32.wordToInt (FoxWord32.rshiftl (seq, 16)))
            val seq_low = FoxWord16.intToWord
	                    (FoxWord32.wordToInt
			     (FoxWord32.andb (seq, ffff)))
	    val ack_high = FoxWord16.intToWord
	                    (FoxWord32.wordToInt (FoxWord32.rshiftl (ack, 16)))
            val ack_low = FoxWord16.intToWord
	                    (FoxWord32.wordToInt
			     (FoxWord32.andb (ack, ffff)))
	    val wnd = FoxWord16.intToWord (FoxWord32.wordToInt window)
	in neg (add (add (wnd, rest),
		     add (add (seq_high, seq_low),
			  add (ack_high, ack_low))))
	end

   fun do_check (data, tlen, peer_sum) =
        let val len_check = FoxWord16.intToWord tlen
	    val check = B.Dyn_Array.checksum (data, 0, tlen)
	in neg (add (add (len_check, peer_sum), check))
	end

   val (check_new, check) = if compute_checksums then (do_check_new, do_check)
			    else (fn _ => zero_check, fn _ => zero_check)

   fun update_header (packet, seq, ack, wnd) =
        let val window = FoxWord16.intToWord (FoxWord32.wordToInt wnd)
	in B.Dyn_Array.update2 (packet, window_index, htons window);
	   B.Dyn_Array.update4 (packet, seq_index, htonl seq);
	   B.Dyn_Array.update4 (packet, ack_index, htonl ack)
	end

   fun print_checksum (peer, packet) =
        let val tlen = B.Dyn_Array.size packet
            val b2tlen = FoxWord16.intToWord tlen
	    val check = B.Dyn_Array.checksum (packet, 0, tlen)
	    val total_check = add (b2tlen, add (peer, check))
	in if total_check = zero_check orelse total_check = ffff_check then ()
	   else
	    local_print ("outgoing checksum for packet " ^
			 FoxMakestring.word16 check ^
			 ", length " ^ FoxMakestring.word16 b2tlen ^
			 ", peer checksum " ^ FoxMakestring.word16 peer ^
			 ", total checksum " ^
			 FoxMakestring.word16 total_check ^
			 " (does not verify)")
	end

   fun add_fast_header (T {header_template, peer_checksum, allocate, ...},
			{seq, ack, wnd, len, data}) =
        let val tlen = FoxWord32.wordToInt len + min_hlen
	    val (packet, send) = allocate tlen
	in B.Dyn_Array.update (packet, 0, header_template);
	   B.Dyn_Array.update (packet, min_hlen, B.Dyn_Array.read data);
	   update_header (packet, seq, ack, wnd);
	   B.Dyn_Array.update2 (packet, cks_index, zero_check);
	   B.Dyn_Array.update2 (packet, cks_index,
				htons (check (packet, tlen, peer_checksum)));
	   Trace.do_if_debug (fn _ => print_checksum (peer_checksum, packet));
	   (packet, send)
	end

   fun add_header (T {local_port, remote_port, peer_checksum, allocate, ...},
		   {seg, data}) =
        let val Tcp_Tcb.Seg {seq, ack, len, wnd, up, options,
			     syn_flag, fin_flag, reset_flag,
			     ack_flag, push_flag, urgent_flag} = seg
	    val hlen = ((header_len options + 3) quot 4) * 4
	    val shifted_hlen = FoxWord8.lshift (FoxWord8.intToWord
						(hlen quot 4), 4)
	    val flags = make_flags (syn_flag, fin_flag, reset_flag,
				    ack_flag, push_flag, urgent_flag)
	    val urgent = FoxWord16.intToWord (FoxWord32.wordToInt up)
	    val tlen = hlen + FoxWord32.wordToInt len
	    val (packet, send) = allocate tlen
	in B.Dyn_Array.update1 (packet, hlen_index, shifted_hlen);
	   B.Dyn_Array.update1 (packet, flag_index, flags);
	   B.Dyn_Array.update2 (packet, urg_index, htons urgent);
	   B.Dyn_Array.update2 (packet, src_index, htons local_port);
	   B.Dyn_Array.update2 (packet, dest_index, htons remote_port);
	   B.Dyn_Array.update2 (packet, cks_index, zero_check);
	   update_header (packet, seq, ack, wnd);
	   write_options (packet, min_hlen, hlen, options);
	   B.Dyn_Array.update (packet, hlen, B.Dyn_Array.read data);
	   B.Dyn_Array.update2 (packet, cks_index,
				htons (check (packet, tlen, peer_checksum)));
	   Trace.do_if_debug (fn _ => print_checksum (peer_checksum, packet));
	   (packet, send)
	end

(*
---------------------------------------------------------------------
	6.	function send_segment
*)

  in

   fun send_segment (T {empty_packet, empty_checksum,
			empty_send, peer_checksum, ...},
		     Tcp_Tcb.Fast_Empty {seq, ack, wnd}) =
        (update_header (empty_packet, seq, ack, wnd);
	 B.Dyn_Array.update2 (empty_packet, cks_index,
			      htons (check_new (seq, ack, wnd,
						empty_checksum)));
	 Trace.do_if_debug (fn _ => 
			    (print_checksum (peer_checksum, empty_packet);
			     print_send (Tcp_Tcb.Fast_Empty
					 {seq = seq, ack = ack, wnd = wnd})));
	 ((empty_send ())
	  handle x =>
	          (local_print "exception in lower send for empty packet";
		   send_exception x)))
     | send_segment (state, Tcp_Tcb.Fast_Out packet) =
        let val (_, send) = add_fast_header (state, packet)
	in Trace.do_if_debug (fn _ => print_send (Tcp_Tcb.Fast_Out packet));
	   ((send ())
	    handle x =>
	            (local_print "exception in lower send for fast packet";
		     send_exception x))
	end (* let *)
     | send_segment (state, Tcp_Tcb.Out_Seg packet) =
        let val (tcp_packet, send) = add_header (state, packet)
	in Trace.do_if_debug (fn _ => print_send (Tcp_Tcb.Out_Seg packet));
	   ((send ())
	    handle x =>
	            (local_print "exception in lower send";
		     send_exception x))
	end (* let *)

(*
---------------------------------------------------------------------
	7.	function send_preallocated
*)

   fun send_preallocated (_, packet, send, tlen, peer)
                         (Tcp_Tcb.Fast_Out {seq, ack, wnd, len, data}) =
	(update_header (packet, seq, ack, wnd);
	 B.Dyn_Array.update2 (packet, cks_index, zero_check);
	 B.Dyn_Array.update2 (packet, cks_index,
			      htons (check (packet, tlen, peer)));
	 Trace.do_if_debug (fn _ => print_checksum (peer, packet));
	 ((send ())
	  handle x =>
	          (local_print "error sending pre-allocated packet";
		   print_send (Tcp_Tcb.Fast_Out {seq = seq, ack = ack,
						  wnd = wnd, len = len,
						  data = data});
		   send_exception x)))
	(* if we're not sending a fast_out,
	   we might as well call send_segment. *)
     | send_preallocated (state, packet, send, tlen, peer) segment =
        send_segment (state, segment)

(*
---------------------------------------------------------------------
	8.	function allocate_segment
*)

   fun allocate_segment (state as (T {allocate, header_template,
				      peer_checksum, ...}),
			 size) =
        let val header = B.Create.copy_create (header_template, 0, min_hlen)
	    val tlen = size + min_hlen
	    val (tcp_packet, send) = allocate tlen
	    val returned_packet = B.Dyn_Array.tail (tcp_packet, min_hlen)
	in B.Dyn_Array.update (tcp_packet, 0, header);
	   (returned_packet,
	    send_preallocated (state, tcp_packet, send,
			       tlen, peer_checksum))
	end (* let *)

(*
---------------------------------------------------------------------
	9.	internal function create_header
*)

   (* create pseudo-state and pseudo-packets, then call add_header. *)
   fun create_header (local_port, remote_port, peer_checksum) =
        let fun nop _ = ()
	    val no_head = B.Create.create min_hlen
	    val no_packet = B.Dyn_Array.new min_hlen
	    val no_data = B.Dyn_Array.empty
	    fun allocate _ = (no_packet, nop)
	    val state = T {allocate = allocate, act = nop,
			   local_port = local_port, remote_port = remote_port,
			   peer_checksum = peer_checksum,
			   header_template = no_head, empty_packet = no_data,
			   empty_checksum = zero_check, empty_send = nop,
			   to_do = ref (Tcp_Tcb.Q.new ())}
	    val zero = SW.n32 "0"
	    val segment = Tcp_Tcb.Seg {seq = zero, ack = zero, len = zero,
				       wnd = zero, up = zero, options = [],
				       syn_flag = false, fin_flag = false,
				       reset_flag = false, ack_flag = true,
				       push_flag = true, urgent_flag = false}
	    val out_seg = {seg = segment, data = no_data}
	    val (packet, _) = add_header (state, out_seg)
	    val header = B.Dyn_Array.read packet
	    val hlen = ByteArray.length header
	    exception Error_In_Tcp_Header_Generation
	in if hlen <> min_hlen then
	    (local_print ("error, generated header template with length " ^
			  FoxMakestring.int hlen ^ " instead of " ^
			  FoxMakestring.int min_hlen);
	     raise Error_In_Tcp_Header_Generation)
	   else
	    header
	end

  end (* local *)

(*
---------------------------------------------------------------------
	10.	function process_packet

	Check that the packet checksum is correct, then return the
	corresponding action (Process_Data).
*)

  local
   fun size_error (opt, from, to) =
        local_print ("received TCP option " ^
		     FoxMakestring.int opt ^ " at illegal offset " ^
		     FoxMakestring.int from ^ ", limit " ^
		     FoxMakestring.int to)

   fun length_error (opt, length, expect) =
        local_print ("received illegal TCP option " ^
		     FoxMakestring.word8 opt ^ " with length " ^
		     FoxMakestring.int length ^ "," ^
		     FoxMakestring.int expect ^ " expected");

   val bad_packet = Tcp_Tcb.Log_Event Tcp_Tcb.Log.Bad_Packet_Received

   fun illegal_length (p, to_do) =
        (local_print ("Illegal length " ^
		      FoxMakestring.int (B.Dyn_Array.size p) ^
		      " for packet");
	 to_do := Tcp_Tcb.Q.add (! to_do, bad_packet))

   val add = B.Checksum.one_s_add

   fun bad_checksum (check, peer_check, pseudo_check, len, hlen, to_do) =
        (local_print ("error, received message has checksum " ^
		      FoxMakestring.word16 check ^ ", pseudo-checksum " ^
		      FoxMakestring.word16 pseudo_check ^ ", length " ^
		      FoxMakestring.int len ^ ", header length " ^
		      FoxMakestring.int hlen ^ ", computed pseudo-checksum " ^
		      FoxMakestring.word16 (add (peer_check,
						 FoxWord16.intToWord len)) ^
		      ", discarding");
	 to_do := Tcp_Tcb.Q.add (! to_do, bad_packet))

   fun length_problem (len, hlen, to_do) =
        (local_print ("error, received TCP message has length " ^
		      FoxMakestring.int len ^
		      ", header length " ^ FoxMakestring.int hlen ^
		      ", discarding");
	 to_do := Tcp_Tcb.Q.add (! to_do, bad_packet))

   val ntohs = B.Order.B2.from_big
   val ntohl = B.Order.B4.from_big

   fun compute_options (header, from, to) =
        if from >= to then []
	else
	 case FoxWord8.wordToInt (FoxWord8.sub (header, from)) of
	    0 => []			(* end_of_option code *)
	  | 1 =>			(* nop_option code *)
	     compute_options (header, from + nop_option_length, to)
	  | 2 =>			(* mss_option code *)
	     if from + mss_option_length > to then
	      (size_error (2, from, to);
	       [])
	     else
	      let val len_index = from + opt_length_offset
	          val mss_index = from + opt_value_offset
		  val optlen = FoxWord8.wordToInt (FoxWord8.sub
						   (header, len_index))
		  val optval = ntohs (FoxWord16.sub (header, mss_index))
	      in if optlen <> mss_option_length then
	          (length_error (SW.n8 "2", optlen, mss_option_length);
		   [])
		 else
		  (Tcp_Tcb.Max_Segment {size = optval} ::
		   compute_options (header, from + optlen, to))
	      end    (* let *)
	  | 3 =>			(* window_scale_option code *)
	     if from + window_scale_option_length > to then
	      (size_error (3, from, to);
	       [])
	     else
	      let val len_index = from + opt_length_offset
	          val shift_index = from + opt_value_offset
		  val optlen = FoxWord8.wordToInt (FoxWord8.sub
						   (header, len_index))
		  val optval = FoxWord8.sub (header, shift_index)
	      in if optlen <> window_scale_option_length then
	          (length_error (SW.n8 "3", optlen,
				 window_scale_option_length);
		   [])
		 else
		  (Tcp_Tcb.Window_Scale {shift = optval} ::
		   compute_options (header, from + optlen, to))
	      end    (* let *)
	  | n =>			(* other options *)
	     (* (RFC 1122, p. 85): this unknown option has a length field. *)
	     if from + opt_length_offset >= to then
	      (size_error (n, from, to);
	       [])
	     else
	      let val len_index = from + opt_length_offset
	          val optlen1 = FoxWord8.sub (header, len_index)
		  val optlen = FoxWord8.wordToInt optlen1
	      in if from + optlen > to then
	          (size_error (n, from, to);
		   [])
		 else		(* ignore this option *)
		  (local_print ("ignoring TCP option " ^
				FoxMakestring.int n ^
				" (not supported)");
		   compute_options (header, from + optlen, to))
	      end   (* let *)

   fun deliver_fast_print input =
        local_print ("received " ^ Tcp_Tcb.in_string input)

   fun deliver_packet_print seg =
        local_print ("received " ^ Tcp_Tcb.segment_string seg)

   val ack_flags = ack_flag
   val ack_push_flags = FoxWord8.orb (ack_flag, psh_flag)
   val non_ack_push_flags = FoxWord8.notb ack_push_flags

   fun deliver_packet (packet, dlen, min_header, hlen, to_do) =
        let val seq = ntohl (FoxWord32.sub (min_header, seq_index))
	    val ack = ntohl (FoxWord32.sub (min_header, ack_index))
	    val wnd2 = ntohs (FoxWord16.sub (min_header, window_index))
	    val wnd = FoxWord32.intToWord (FoxWord16.wordToInt wnd2)
	    val flags = FoxWord8.sub (min_header, flag_index)
	    val data = B.Dyn_Array.tail (packet, hlen)
	in if FoxWord8.andb (flags, non_ack_push_flags) = SW.n8 "0" andalso
	      hlen = min_hlen then    (* fast path *)
	    let val in_seg = Tcp_Tcb.Fast_In {seq = seq, ack = ack, wnd = wnd,
					      len = FoxWord32.intToWord dlen,
					      data = data}
	    in Trace.do_if_debug (fn _ => deliver_fast_print in_seg);
	       to_do := Tcp_Tcb.Q.add (! to_do, Tcp_Tcb.Process_Data in_seg)
	    end (* let *)
	   else
	    let val header = B.Dyn_Array.sub (packet, 0, hlen)
	        val up2 = ntohs (FoxWord16.sub (header, urg_index))
	        val up = FoxWord32.intToWord (FoxWord16.wordToInt up2)
	        val opts = if hlen = min_hlen then [] (* fast path *)
			   else compute_options (header, min_hlen, hlen)
		val s = FoxWord8.andb (flags, syn_flag) <> no_flag
	        val f = FoxWord8.andb (flags, fin_flag) <> no_flag
	        val r = FoxWord8.andb (flags, rst_flag) <> no_flag
	        val a = FoxWord8.andb (flags, ack_flag) <> no_flag
	        val p = FoxWord8.andb (flags, psh_flag) <> no_flag
	        val u = FoxWord8.andb (flags, urg_flag) <> no_flag
	        val seg = Tcp_Tcb.Seg {seq = seq, ack = ack,
				       len = FoxWord32.intToWord dlen,
				       wnd = wnd, up = up,
				       options = opts,
				       syn_flag = s, fin_flag = f,
				       reset_flag = r, ack_flag = a,
				       push_flag = p, urgent_flag = u}
		val in_seg = Tcp_Tcb.In_Seg {seg = seg, data = data}
	    in Trace.do_if_debug (fn _ => deliver_packet_print seg);
	       to_do := Tcp_Tcb.Q.add (! to_do, Tcp_Tcb.Process_Data in_seg)
	    end (* let *)
	end (* let *)

   val add = B.Checksum.one_s_add

  in (* local *)

   fun process_packet (T {to_do, peer_checksum, ...},
		       packet, pseudo_check, min_header) =
        (let val packet_size = B.Dyn_Array.size packet
	     val hlen_byte = FoxWord8.sub (min_header, hlen_index)
	     val hlen = FoxWord8.wordToInt (FoxWord8.*
					    (SW.n8 "4",
					     FoxWord8.rshiftl (hlen_byte, 4)))
	 in if packet_size >= hlen andalso hlen >= min_hlen then
	     if compute_checksums then
	      let val data_check = B.Dyn_Array.checksum (packet, 0,
							 packet_size)
		  val check = add (data_check, pseudo_check)
	      in if check = zero_check orelse check = ffff_check then
		  deliver_packet (packet, packet_size - hlen,
				  min_header, hlen, to_do)
		 else
		  bad_checksum (check, peer_checksum, pseudo_check,
				packet_size, hlen, to_do)
	      end (* let *)
	     else  (* no checksum computation *)
	      deliver_packet (packet, packet_size - hlen,
			      min_header, hlen, to_do)
	    else  (* some length problem *)
	     length_problem (packet_size, hlen, to_do)
	 end) (* let *)
       handle x =>
	       local_print ("exception " ^ System.exn_name x ^
			    " in process_packet")

  end (* local *)

(*
---------------------------------------------------------------------
	11.	function new
*)

  fun new {allocate, act, to_do,
	   address = Address {local_port, remote_port, peer_checksum}} =
       let val header = create_header (local_port, remote_port, peer_checksum)
	   val (empty_packet, empty_send) = allocate min_hlen
	   val empty_header = B.Create.copy_create (header, 0, min_hlen)
	   val add = B.Checksum.one_s_add 
       in FoxWord8.update (empty_header, flag_index, ack_flag);
          FoxWord16.update (empty_header, cks_index, zero_check);
	  B.Dyn_Array.update (empty_packet, 0, empty_header);
	  T {allocate = allocate, act = act,
	     local_port = local_port, remote_port = remote_port,
	     peer_checksum = peer_checksum,
	     header_template = header, empty_packet = empty_packet,
	     empty_checksum = add (B.Dyn_Array.checksum (empty_packet, 0,
							 min_hlen),
				   add (peer_checksum,
					FoxWord16.intToWord min_hlen)),
	     empty_send = empty_send, to_do = to_do}
       end

 end (* struct *)

