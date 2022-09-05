(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edo Biagioni (esb@cs.cmu.edu)
	Ken Cline (ken.cline@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

	i.	Abstract

	tcpheader.fun: the implementation of the actions specified
	by the TCP finite state machine. The actions are those
	defined in tcptcb.sig.


	ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor Tcp_Header
	2.	internal TCP flag bits
	3.	internal TCP header definition
	4.	internal function add_header
	5.	function send_segment
	6.	internal function create_header
	7.	function process_packet
	8.	function identify
	9.	function new


	iii.	RCS Log

$Log: tcpheader.fun,v $
Revision 1.14  1997/05/29  11:04:44  esb
fixed bugs in the slow unmarshaling.

Revision 1.13  97/04/22  11:23:46  esb
optimized.

Revision 1.12  96/05/14  01:24:38  esb
changed to support TCP timestamp options.

Revision 1.11  1996/03/15  22:46:04  esb
optimized the computation of checksums.

Revision 1.10  1996/03/12  22:24:59  esb
added printing of entire packet when checksum is bad.

Revision 1.9  1996/02/29  17:34:53  esb
improved the printing of messages with incorrect checksum.

Revision 1.8  1996/01/19  23:03:30  esb
adapted to the new wordarray signature.

Revision 1.7  1996/01/15  18:00:49  cline
removed FoxWord

Revision 1.6  1995/11/08  23:10:27  cline
*** empty log message ***

Revision 1.5  1995/09/26  13:06:25  cline
removed erroneous byte swapping.

Revision 1.4  1995/09/20  19:51:28  cline
removed debugging stuff

Revision 1.3  1995/09/14  21:11:00  cline
work around for representation bug

Revision 1.2  1995/08/08  19:05:21  cline
*** empty log message ***

Revision 1.1  1995/08/08  18:38:53  cline
Initial revision


	1.	functor Tcp_Header
*)

functor Tcp_Header (structure Tcp_Tcb: TCP_TCB
		    structure Incoming: NETWORK_INCOMING
		    structure Outgoing: EXTERNAL
		    structure B: FOX_BASIS
		    sharing type Tcp_Tcb.incoming_data = Incoming.T
			and type Tcp_Tcb.outgoing_data = Outgoing.T
		    val compute_checksums: bool
		    val send_exception: exn -> unit
		    val debug_level: int ref option): TCP_HEADER =
 struct
  structure Trace = Trace (structure V = B.V
			   val debug_level = debug_level
			   val module_name = "tcpheader.fun"
			   val makestring = fn _ => NONE)
  val local_print = Trace.local_print

  structure Word8_In = Protocol_Extern8 (structure In = Incoming
					 structure Out = Outgoing
					 structure B = B)
  structure Word16_In = Protocol_Extern16_Big (structure In = Incoming
					       structure Out = Outgoing
					       structure B = B)
  structure Word32_In = Protocol_Extern32_Big (structure In = Incoming
					       structure Out = Outgoing
					       structure B = B)
  structure Word8_Out = Protocol_Extern8 (structure In = Incoming
					  structure Out = Outgoing
					  structure B = B)
  structure Word16_Out = Protocol_Extern16_Big (structure In = Incoming
						structure Out = Outgoing
						structure B = B)
  structure Word32_Out = Protocol_Extern32_Big (structure In = Incoming
						structure Out = Outgoing
						structure B = B)

  type segment = Tcp_Tcb.tcp_out
  type send_packet = Outgoing.T
  type receive_packet = Incoming.T
  type user_packet = Incoming.T
  type to_do_list = Tcp_Tcb.tcp_action Tcp_Tcb.Q.T ref
  datatype address = Address of {local_port: Word16.word,
				 remote_port: Word16.word}

  datatype action_state =
        T of {act: (unit -> unit),
	      local_port: Word16.word,
	      remote_port: Word16.word,
	      peer_checksum: Outgoing.T -> Word16.word,
	      header_template: Outgoing.T,
	      empty_packet: Outgoing.T,
	      empty_checksum: Word16.word,
	      header_template_with_ts_option: Outgoing.T,
	      empty_packet_with_ts_option: Outgoing.T,
	      empty_with_ts_checksum: Word16.word,
	      to_do: to_do_list}

(*
	2.	internal TCP flag bits
 *)

  val no_flag  = 0wx00 : Word8.word
  val fin_flag = 0wx01 : Word8.word
  val syn_flag = 0wx02 : Word8.word
  val rst_flag = 0wx04 : Word8.word
  val psh_flag = 0wx08 : Word8.word
  val ack_flag = 0wx10 : Word8.word
  val urg_flag = 0wx20 : Word8.word

(*
	3.	internal TCP header definition
 *)

  val min_hlen = 0w20
  val src_index = 0w0
  val dest_index = 0w2
  val seq_index = 0w4
  val ack_index = 0w8
  val hlen_index = 0w12
  val flag_index = 0w13
  val window_index = 0w14
  val cks_index = 0w16
  val urg_index = 0w18
  val send_time_index = 0w24		(* RFC 1323, p. 27 *)
  val echo_index = 0w28			(* RFC 1323, p. 27 *)

(* max_hlen is used for sending the max-segment-size option in any packet
   that carries a SYN flag. Our max segment size is the network's MTU.
   The opt_*_offset values are with respect to the beginning of
   each option. Maximum Segment Size is the only non-trivial option. *)
  val max_hlen = min_hlen + 0w4
  val opt_kind_offset = 0w0
  val opt_length_offset = 0w1
  val opt_value_offset = 0w2

  val eoo_option_code = 0w0		(* end-of-option *)
  val eoo_option_length = 0w1
  val eoo_option_code8 = Word8.fromInt (Word.toInt eoo_option_code)
  val eoo_option_length8 = Word8.fromInt (Word.toInt eoo_option_length)

  val nop_option_code = 0w1		(* no-op *)
  val nop_option_length = 0w1
  val nop_option_code8 = Word8.fromInt (Word.toInt nop_option_code)
  val nop_option_length8 = Word8.fromInt (Word.toInt nop_option_length)

  val mss_option_code = 0w2
  val mss_option_length = 0w4
  val mss_option_code8 = Word8.fromInt (Word.toInt mss_option_code)
  val mss_option_length8 = Word8.fromInt (Word.toInt mss_option_length)

  val window_scale_option_code = 0w3
  val window_scale_option_length = 0w3
  val window_scale_option_code8 =
        Word8.fromInt (Word.toInt window_scale_option_code)
  val window_scale_option_length8 =
        Word8.fromInt (Word.toInt window_scale_option_length)

  val ts_option_code = 0w8		(* RFC 1323, p. 13 *)
  val ts_option_length = 0w10
  val ts_option_padded_length = 0w12	(* RFC 1323, p. 27 *)
  val ts_option_code8 = Word8.fromInt (Word.toInt ts_option_code)
  val ts_option_length8 = Word8.fromInt (Word.toInt ts_option_length)

(* RFC 1323, p. 27 defines an "optional standard" encoding to be
   used for fast path processing.  We recognize it on incoming packets. *)
  val ts_hlen = min_hlen + ts_option_padded_length
  val w8to32 = Word32.fromInt o Word8.toInt
  fun mkbyte (high, second, third, low) =
       Word32.orb (Word32.orb (Word32.<< (w8to32 high, 0w24),
			       Word32.<< (w8to32 second, 0w16)),
		   Word32.orb (Word32.<< (w8to32 third, 0w8), w8to32 low))
  val fast_ts_option_word = mkbyte (nop_option_code8, nop_option_code8,
				    ts_option_code8, ts_option_length8)

(*
	4.	internal function add_header

	Make_header generates a header for the specified packet.  The
	checksum is the correct checksum for the packet.
*)

  val zero_check = Word16.fromInt 0
  val ffff_check = Word16.fromInt 0xffff
  val zero32 = Word32.fromInt 0

  local
   fun header_len [] = min_hlen
     | header_len ((Tcp_Tcb.Max_Segment {size}) :: rest) =
        mss_option_length + header_len rest
     | header_len ((Tcp_Tcb.Window_Scale {shift}) :: rest) =
        window_scale_option_length + header_len rest
     | header_len ((Tcp_Tcb.Timestamp {send_time, echo}) :: rest) =
        ts_option_padded_length + header_len rest
(* redundant match, may be required if Tcp_Tcb declares more TCP options.
     | header_len (_ :: rest) = header_len rest
*)

   fun write_options (packet, offset, hlen, []) =
        if offset < hlen then
         (Word8_Out.marshal
	    (packet, Word8.fromInt (Word.toInt eoo_option_code)) offset;
	  write_options (packet, offset + eoo_option_length, hlen, []))
	else ()
     | write_options (packet, offset, hlen,
		      Tcp_Tcb.Max_Segment {size} :: rest) =
	write_options (packet, 
		       ((Word16_Out.marshal (packet, size)) o
			(Word8_Out.marshal (packet, mss_option_length8)) o
			(Word8_Out.marshal (packet, mss_option_code8))) offset,
		       hlen, rest)
     | write_options (packet, offset, hlen,
		      Tcp_Tcb.Window_Scale {shift} :: rest) =
	write_options (packet,
		       ((Word8_Out.marshal (packet, shift)) o
			(Word8_Out.marshal (packet,
					    window_scale_option_length8)) o
			(Word8_Out.marshal (packet,
					    window_scale_option_code8)))
		       offset, hlen, rest)
(* for alignment, we write two no-op options before the timestamp option.
   This is recommended in RFC 1323, p. 27. *)
     | write_options (packet, offset, hlen,
		      Tcp_Tcb.Timestamp {send_time, echo} :: rest) =
	write_options (packet,
		       ((Word32_Out.marshal (packet, echo)) o
		        (Word32_Out.marshal (packet, send_time)) o
			(Word8_Out.marshal (packet, ts_option_length8)) o
			(Word8_Out.marshal (packet, ts_option_code8)) o
			(Word8_Out.marshal (packet, nop_option_code8)) o
			(Word8_Out.marshal (packet, nop_option_code8))) offset,
		       hlen, rest)
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

   val ack_push = Word8.orb (ack_flag, psh_flag)

   fun make_flags (false, false, false, true, true, false) = ack_push
     | make_flags (false, false, false, true, false, false) = ack_flag
     | make_flags (s, f, r, a, p, u) =
        Word8.orb (Word8.orb (Word8.orb (make_syn s, make_fin f),
				    Word8.orb (make_rst r, make_ack a)),
		      Word8.orb (make_psh p, make_urg u))

   val add = B.Checksum.one_s_add
   val neg = B.Checksum.one_s_complement

   val ffff = Word32.fromInt 0xffff

   val w32to16 = Word16.fromInt o Word32.toInt

   fun split_high_low value =
        (w32to16 (Word32.>> (value, 0w16)),
	 w32to16 (Word32.andb (value, ffff)))

   fun do_check_new (seq, ack, window, rest) =
        let val (seq_high, seq_low) = split_high_low seq
            val (ack_high, ack_low) = split_high_low ack
	    val wnd = w32to16 window
	in neg (add (rest,
		     add (wnd, add (add (seq_high, seq_low),
				    add (ack_high, ack_low)))))
	end

   fun do_check_new_ts (seq, ack, window, {send_time, echo}, rest) =
        let val (seq_high, seq_low) = split_high_low seq
            val (ack_high, ack_low) = split_high_low ack
            val (send_time_high, send_time_low) = split_high_low send_time
            val (echo_high, echo_low) = split_high_low echo
	    val wnd = w32to16 window
	in neg (add (add (rest, wnd),
		     add (add (add (seq_high, seq_low),
			       add (ack_high, ack_low)),
			  add (add (send_time_high, send_time_low),
			       add (echo_high, echo_low)))))
	end

   fun do_check (packet, peer_sum) =
        let val start = B.Checksum.initial_state
	    val fold = Outgoing.fold (packet, B.Checksum.check_partial, start)
	    val check = B.Checksum.complete_partial fold
	    val total_check = neg (add (peer_sum, check))
	in total_check
	end

   val (check, check_new, check_new_ts) =
         if compute_checksums then (do_check, do_check_new, do_check_new_ts)
	 else (fn _ => zero_check, fn _ => zero_check, fn _ => zero_check)

   fun update_header (packet, seq, ack, wnd) =
        let val window = Word16.fromInt (Word32.toInt wnd)
	in Word16_Out.marshal (packet, window) window_index;
	   Word32_Out.marshal (packet, seq) seq_index;
	   Word32_Out.marshal (packet, ack) ack_index
	end

   fun update_header_ts (packet, seq, ack, wnd, {send_time, echo}) =
        let val window = Word16.fromInt (Word32.toInt wnd)
	in Word16_Out.marshal (packet, window) window_index;
	   Word32_Out.marshal (packet, seq) seq_index;
	   Word32_Out.marshal (packet, ack) ack_index;
	   Word32_Out.marshal (packet, send_time) send_time_index;
	   Word32_Out.marshal (packet, echo) echo_index
	end

   fun print_checksum (s, peer, packet) =
        let val total_check = check (packet, peer)
	in if total_check = zero_check orelse total_check = ffff_check then ()
	   else
	    local_print (s ^ ", peer checksum/" ^
			 (Integer.toString o Word16.toInt) peer ^
			 ", total checksum " ^
			 (Integer.toString o Word16.toInt) total_check ^
			 " does not verify:\npacket is " ^
			 Outgoing.makestring packet)
	end

   fun add_fast_header (T {header_template, peer_checksum, ...},
			{seq, ack, wnd, len, data}) =
        let val packet = Outgoing.join (header_template, data)
	    val tlen = Outgoing.size packet
	in update_header (packet, seq, ack, wnd);
	   Word16_Out.marshal (packet, zero_check) cks_index;
	   Word16_Out.marshal (packet, check (packet, peer_checksum packet))
	                      cks_index;
	   Trace.do_if_debug (fn _ =>
			      print_checksum ("add_fast_header",
					      peer_checksum packet, packet));
	   packet
	end

   fun add_ts_header (T {header_template_with_ts_option, peer_checksum, ...},
		      {seq, ack, wnd, len, data, times}) =
        let val packet = Outgoing.join (header_template_with_ts_option, data)
	    val tlen = Outgoing.size packet
	in update_header_ts (packet, seq, ack, wnd, times);
	   Word16_Out.marshal (packet, zero_check) cks_index;
	   Word16_Out.marshal (packet, check (packet, peer_checksum packet))
	                      cks_index;
	   Trace.do_if_debug (fn _ =>
			      print_checksum ("add_ts_header",
					      peer_checksum packet, packet));
	   packet
	end

   fun add_header (T {local_port, remote_port, peer_checksum, ...},
		   {seg, data}) =
        let val Tcp_Tcb.Seg {seq, ack, len, wnd, up, options,
			     syn_flag, fin_flag, reset_flag,
			     ack_flag, push_flag, urgent_flag} = seg
	    val hlen = ((header_len options + 0w3) div 0w4) * 0w4
	    val shifted_hlen = Word8.<< (Word8.fromInt
					 (Word.toInt (hlen div 0w4)), 0w4)
	    val flags = make_flags (syn_flag, fin_flag, reset_flag,
				    ack_flag, push_flag, urgent_flag)
	    val urgent = Word16.fromInt (Word32.toInt up)
	    val packet = Outgoing.join (Outgoing.uninitialized hlen, data)
	    val tlen = Outgoing.size packet
	in Word8_Out.marshal (packet, shifted_hlen) hlen_index;
	   Word8_Out.marshal (packet, flags) flag_index;
	   Word16_Out.marshal (packet, urgent) urg_index;
	   Word16_Out.marshal (packet, local_port) src_index;
	   Word16_Out.marshal (packet, remote_port) dest_index;
	   Word16_Out.marshal (packet, zero_check) cks_index;
	   update_header (packet, seq, ack, wnd);
	   write_options (packet, min_hlen, hlen, options);
	   Word16_Out.marshal (packet, check (packet, peer_checksum packet))
	                      cks_index;
	   Trace.do_if_debug (fn _ =>
			      print_checksum ("add_header",
					      peer_checksum packet, packet));
	   packet
	end

   fun debug_print_send (p, packet, peer_checksum, s) _ =
        (print_checksum (s, peer_checksum packet, packet);
	 Trace.local_print ("send " ^ Tcp_Tcb.out_string p))

   fun debug_print_state (p, packet, T {peer_checksum, ...}, s) _ =
        (print_checksum (s, peer_checksum packet, packet);
	 Trace.local_print ("send " ^ Tcp_Tcb.out_string p))

(*
	5.	function send_segment
*)

  in

   fun send_segment (T {empty_packet, empty_checksum, peer_checksum, ...},
		     p as (Tcp_Tcb.Fast_Empty {seq, ack, wnd}), lower_send) =
        (update_header (empty_packet, seq, ack, wnd);
	 Word16_Out.marshal (empty_packet,
			     check_new (seq, ack, wnd, empty_checksum))
			    cks_index;
	 Trace.do_if_debug (debug_print_send (p, empty_packet, peer_checksum,
					      "send_segment-1"));
	 ((lower_send empty_packet)
	  handle x =>
	          (local_print "exception in lower send for empty packet";
		   send_exception x)))
     | send_segment (T {empty_packet_with_ts_option, empty_with_ts_checksum,
			peer_checksum, ...},
		     p as (Tcp_Tcb.Timestamp_Empty {seq, ack, wnd, times}),
		     lower_send) =
        (update_header_ts (empty_packet_with_ts_option, seq, ack, wnd, times);
	 Word16_Out.marshal (empty_packet_with_ts_option,
			     check_new_ts (seq, ack, wnd, times,
					   empty_with_ts_checksum))
			    cks_index;
	 Trace.do_if_debug (debug_print_send (p, empty_packet_with_ts_option,
					      peer_checksum,
					      "send_segment-2"));
	 ((lower_send empty_packet_with_ts_option)
	  handle x =>
	          (local_print "exception in lower send for empty ts packet";
		   send_exception x)))
     | send_segment (state, p as (Tcp_Tcb.Fast_Out packet), lower_send) =
        let val tcp_packet = add_fast_header (state, packet)
	in Trace.do_if_debug (debug_print_state (p, tcp_packet, state,
						 "send_segment-3"));
	   (lower_send tcp_packet
	    handle x =>
	            (local_print "exception in lower send for fast packet";
		     send_exception x))
	end (* let *)
     | send_segment (state, p as (Tcp_Tcb.Timestamp_Out packet), lower_send) =
        let val tcp_packet = add_ts_header (state, packet)
	in Trace.do_if_debug (debug_print_state (p, tcp_packet, state,
						 "send_segment-4"));
	   (lower_send tcp_packet
	    handle x =>
	            (local_print "exception in lower send for ts packet";
		     send_exception x))
	end (* let *)
     | send_segment (state, p as (Tcp_Tcb.Out_Seg packet), lower_send) =
        let val tcp_packet = add_header (state, packet)
	in Trace.do_if_debug (debug_print_state (p, tcp_packet, state,
						 "send_segment-5"));
	   (lower_send tcp_packet
	    handle x =>
	            (local_print "exception in lower send";
		     send_exception x))
	end (* let *)

(*
	6.	internal function create_header
*)

   (* create pseudo-state and pseudo-packets, then call add_header. *)
   fun create_header (local_port, remote_port, peer_checksum, options) =
        let fun nop _ = ()
	    val no_head = Outgoing.uninitialized min_hlen
	    val no_packet = Outgoing.uninitialized min_hlen
	    val no_data = Outgoing.uninitialized 0w0
	    fun allocate _ = (no_packet, nop)
	    val state = T {act = nop, local_port = local_port,
			   remote_port = remote_port,
			   peer_checksum = peer_checksum,
			   header_template = no_head, empty_packet = no_data,
			   empty_checksum = zero_check,
			   header_template_with_ts_option = no_head,
			   empty_packet_with_ts_option = no_data,
			   empty_with_ts_checksum = zero_check,
			   to_do = ref (Tcp_Tcb.Q.new ())}
	    val segment = Tcp_Tcb.Seg {seq = zero32, ack = zero32,
				       len = zero32,
				       wnd = zero32, up = zero32,
				       options = options,
				       syn_flag = false, fin_flag = false,
				       reset_flag = false, ack_flag = true,
				       push_flag = true, urgent_flag = false}
	    val out_seg = {seg = segment, data = no_data}
	    val header = add_header (state, out_seg)
	    val hlen = Outgoing.size header
	    exception Error_In_Tcp_Header_Generation
	in case options of
	    [] =>
	     if hlen <> min_hlen then
	      (local_print ("error, generated header template with length " ^
			    Word.toString hlen ^ " instead of " ^
			    Word.toString min_hlen);
	       raise Error_In_Tcp_Header_Generation)
	     else header
	  | [Tcp_Tcb.Timestamp _] =>
	     if hlen <> min_hlen + ts_option_padded_length then
	      (local_print ("error, generated header template with length " ^
			    Word.toString hlen ^ " instead of " ^
			    Word.toString (min_hlen +
					   ts_option_padded_length));
	       raise Error_In_Tcp_Header_Generation)
	     else header
	  | _ =>
	     (local_print "error, requested header with illegal options";
	      raise Error_In_Tcp_Header_Generation)
	end

  end (* local *)

(*
	7.	function process_packet

	Check that the packet checksum is correct, then return the
	corresponding action (Process_Data).
*)

  val w32x1f = Word32.fromInt 0x1f
  val w32xffff = Word32.fromInt 0xffff

  local
   fun size_error (opt, from, to) =
        local_print ("received TCP option " ^
		     Integer.toString opt ^ " at illegal offset " ^
		     Integer.toString (Word.toInt from) ^ ", limit " ^
		     Integer.toString (Word.toInt to))

   fun length_error (opt, length, expect) =
        local_print ("received illegal TCP option " ^
		     Word8.fmt StringCvt.DEC opt ^ " with length " ^
		     Int.toString (Word8.toInt length) ^ "," ^
		     Int.toString (Word.toInt expect) ^ " expected");

   val bad_packet = Tcp_Tcb.Log_Event Tcp_Tcb.Log.Bad_Packet_Received

   fun illegal_length (p, to_do) =
        (local_print ("Illegal length " ^
		      Word.toString (Incoming.size p) ^
		      " for packet");
	 to_do := Tcp_Tcb.Q.add (! to_do, bad_packet))

   val add = B.Checksum.one_s_add

   fun bad_checksum (check, peer_check, hlen, to_do, packet) =
        let val makestring16 = Integer.toString o Word16.toInt
	    val makestring = Integer.toString o Word.toInt
	    val len = Incoming.size packet
	    val pseudo_check = add (peer_check,
				    Word16.fromInt (Word.toInt len))
        in local_print ("error, received message has " ^
			"checksum " ^ makestring16 check ^
			", peer-checksum " ^ makestring16 peer_check ^
			", length " ^ makestring len ^
			", header length " ^ makestring hlen ^
			", pseudo-checksum " ^ makestring16 pseudo_check ^
			", discarding message " ^
			Incoming.makestring packet);
	   to_do := Tcp_Tcb.Q.add (! to_do, bad_packet)
	end

   fun length_problem (packet, hlen, to_do) =
        (local_print ("error, received TCP message has length " ^
		      Int.toString (Word.toInt (Incoming.size packet)) ^
		      ", header length " ^
		      Int.toString (Word.toInt hlen) ^
		      ", discarding");
	 to_do := Tcp_Tcb.Q.add (! to_do, bad_packet))

   fun compute_options (header, from, to) =
        if from >= to then []
	else
	 let val (code, index) = Word8_In.unmarshal (header, from)
	 in case Word8.toInt code of
	       0 => []			(* end_of_option code *)
	     | 1 =>			(* nop_option code *)
	        compute_options (header, index, to)
	     | 2 =>			(* mss_option code *)
	        if from + mss_option_length > to then
	         (size_error (2, from, to);
	          [])
	        else
	         let val (optlen, val_index) =
		             Word8_In.unmarshal (header, index)
		     val (optval, final) =
		             Word16_In.unmarshal (header, val_index)
	         in if optlen <> mss_option_length8 then
	             (length_error (0w2, optlen, mss_option_length);
		      [])
		    else
		     (Tcp_Tcb.Max_Segment {size = optval} ::
		      compute_options (header, final, to))
	         end    (* let *)
	     | 3 =>			(* window_scale_option code *)
	        if from + window_scale_option_length > to then
	         (size_error (3, from, to);
	          [])
	        else
	         let val (optlen, val_index) =
		             Word8_In.unmarshal (header, index)
		     val (optval, final) =
		             Word8_In.unmarshal (header, val_index)
	         in if optlen <> window_scale_option_length8 then
	             (length_error (0w3, optlen, window_scale_option_length);
		      [])
		    else
		     (Tcp_Tcb.Window_Scale {shift = optval} ::
		      compute_options (header, final, to))
	         end    (* let *)
	     | 8 =>			(* ts_option code *)
	        if from + ts_option_length > to then
	         (size_error (8, from, to);
	          [])
	        else
	         let val (optlen, val_index) =
		           Word8_In.unmarshal (header, index)
		     val (v, echo_index) =
		           Word32_In.unmarshal (header, val_index)
		     val (e, final) = Word32_In.unmarshal (header, echo_index)
	         in if optlen <> ts_option_length8 then
	             (length_error (0w8, optlen, ts_option_length);
		      [])
		    else
		     (Tcp_Tcb.Timestamp {send_time = v, echo = e} ::
		      compute_options (header, final, to))
	         end    (* let *)
	     | n =>			(* other options *)
        (* (RFC 1122, p. 85): this unknown option has a length field. *)
	        if from + opt_length_offset >= to then
	         (size_error (n, from, to);
	          [])
	        else
	         let val len_index = from + opt_length_offset
	             val optlen1 = #1 (Word8_In.unmarshal (header, len_index))
		     val optlen = Word.fromInt (Word8.toInt optlen1)
	         in if from + optlen > to then
	             (size_error (n, from, to);
		      [])
		    else		(* ignore this option *)
		     (local_print ("ignoring TCP option " ^
				   Integer.toString n ^
				   " (not supported)");
		      compute_options (header, from + optlen, to))
	         end   (* let *)
	 end   (* let *)

   fun deliver_fast_print input =
        local_print ("received " ^ Tcp_Tcb.in_string input)

   fun build_packet_print seg =
        local_print ("received " ^ Tcp_Tcb.segment_string seg)

   val ack_flags = ack_flag
   val ack_push_flags = Word8.orb (ack_flag, psh_flag)
   val non_ack_push_flags = Word8.notb ack_push_flags

   fun first_option_word packet =
        let val (word, _) = Word32_In.unmarshal (packet, min_hlen)
	in word
	end

   fun expose_fold (wa, l) = (Word_Array.expose wa) :: l

   fun expose_word_arrays array = Incoming.fold (array, expose_fold, [])

   fun is_fast_path (hlen, packet) =
	hlen = min_hlen orelse
	(hlen = ts_hlen andalso first_option_word packet = fast_ts_option_word)

   fun slow_build_packet (packet, dlen, hlen, to_do) =
        let val (seq, _) = Word32_In.unmarshal (packet, seq_index)
	    val (ack, _) = Word32_In.unmarshal (packet, ack_index)
	    val (wnd2, _) = Word16_In.unmarshal (packet, window_index)
	    val wnd = Word32.fromInt (Word16.toInt wnd2)
	    val (flags, _) = Word8_In.unmarshal (packet, flag_index)
	    val (header, headless) = Incoming.split (packet, hlen)
	    val (data, _) = Incoming.split (headless, dlen)
	    val (up2, _) = Word16_In.unmarshal (header, urg_index)
	    val up = Word32.fromInt (Word16.toInt up2)
	    val opts = compute_options (header, min_hlen, hlen)
	    val s = Word8.andb (flags, syn_flag) <> no_flag
	    val f = Word8.andb (flags, fin_flag) <> no_flag
	    val r = Word8.andb (flags, rst_flag) <> no_flag
	    val a = Word8.andb (flags, ack_flag) <> no_flag
	    val p = Word8.andb (flags, psh_flag) <> no_flag
	    val u = Word8.andb (flags, urg_flag) <> no_flag
	    val seg = Tcp_Tcb.Seg {seq = seq, ack = ack,
				   len = Word32.fromInt (Word.toInt dlen),
				   wnd = wnd, up = up,
				   options = opts,
				   syn_flag = s, fin_flag = f,
				   reset_flag = r, ack_flag = a,
				   push_flag = p, urgent_flag = u}
	    val in_seg = Tcp_Tcb.In_Seg {seg = seg, data = data}
	in Trace.do_if_debug (fn _ => build_packet_print seg);
	   to_do := Tcp_Tcb.Q.add (! to_do, Tcp_Tcb.Process_Data in_seg)
	end (* let *)

   fun build_packet (packet, array, index, hlen, w4, to_do) =
        let val seq (* w2 *) = Pack32Big.subArr (array, index + 1)
            val ack (* w3 *) = Pack32Big.subArr (array, index + 2)
	    val wnd = Word32.andb (w4, w32xffff)
	    val flags = Word8.fromLargeWord
	                   (Word32.andb (Word32.>> (w4, 0w16), w32x1f))
	    val (header, data) = Incoming.split (packet, hlen)
	in if Word8.andb (flags, non_ack_push_flags) = 0w0 andalso
	      is_fast_path (hlen, packet) then
	    if hlen = min_hlen then    (* fast path 1 *)
	     let val len = Word.toLargeWord (Incoming.size data)
	         val seg = {seq = seq, ack = ack, wnd = wnd, len = len,
			    data = data, times = NONE}
	         val in_seg = Tcp_Tcb.Fast_In seg
	     in Trace.do_if_debug (fn _ => deliver_fast_print in_seg);
	        to_do := Tcp_Tcb.Q.add (! to_do, Tcp_Tcb.Process_Data in_seg)
	     end (* let *)
	    else			(* fast path 2 *)
	     let val len = Word.toLargeWord (Incoming.size data)
	         val (send_time, echo_cursor) =
		        Word32_In.unmarshal (packet, send_time_index)
	         val (echo, _) = Word32_In.unmarshal (packet, echo_cursor)
	         val seg = {seq = seq, ack = ack, wnd = wnd, len = len,
			    data = data,
			    times = SOME {send_time = send_time, echo = echo}}
		 val in_seg = Tcp_Tcb.Fast_In seg
	     in Trace.do_if_debug (fn _ => deliver_fast_print in_seg);
	        to_do := Tcp_Tcb.Q.add (! to_do, Tcp_Tcb.Process_Data in_seg)
	     end
	   else				(* slow path *)
	    slow_build_packet (packet, Incoming.size packet - hlen,
			       hlen, to_do)
	end (* let *)

   val add = B.Checksum.one_s_add

   fun slow_process_packet (T {to_do, peer_checksum, ...}, packet) =
        let val packet_size = Incoming.size packet
	    val hlen_byte = #1 (Word8_In.unmarshal (packet, hlen_index))
	    val hlen = Word.fromInt (Word8.toInt
				     (Word8.* (0w4,
					       Word8.>> (hlen_byte, 0w4))))
	in if packet_size >= hlen andalso hlen >= min_hlen then
	    if compute_checksums then
	     let val start = B.Checksum.initial_state
	         val fold = Incoming.fold (packet, B.Checksum.check_partial,
					   start)
		 val data_check = B.Checksum.complete_partial fold
		 val check = add (data_check,
				  Incoming.pseudo_header_checksum packet)
	     in if check = zero_check orelse check = ffff_check then
		 slow_build_packet (packet, packet_size - hlen, hlen, to_do)
		else
		 bad_checksum (check, Incoming.pseudo_header_checksum packet,
			       hlen, to_do, packet)
	     end (* let *)
	    else  (* no checksum computation *)
	     slow_build_packet (packet, packet_size - hlen, hlen, to_do)
	   else  (* some length problem *)
	    length_problem (packet, hlen, to_do)
	 end (* let *)

  in (* local *)

   fun process_packet (state as (T {to_do, peer_checksum, ...}), packet) =
	(case expose_word_arrays packet of
	    [(array, first, last)] =>	(* normal, fast case *)
	     if first mod 0w4 = 0w0 then
	      let val index = Word.toInt first div 4
	          val w4 = Pack32Big.subArr (array, index + 3)
	          val hlen = Word.fromLargeWord (Word32.>> (w4, 0w28) * 0w4)
	      in if hlen >= min_hlen then
	          if compute_checksums then
		   let val start = B.Checksum.initial_state
	               val fold = Incoming.fold (packet,
						 B.Checksum.check_partial,
					         start)
		       val data_check = B.Checksum.complete_partial fold
		       val check = add (data_check,
				        Incoming.pseudo_header_checksum packet)
	           in if check = zero_check orelse check = ffff_check then
		       build_packet (packet, array, index, hlen, w4, to_do)
		      else
		       bad_checksum (check,
				     Incoming.pseudo_header_checksum packet,
				     hlen, to_do, packet)
	           end (* let *)
	          else  (* no checksum computation *)
	           build_packet (packet, array, index, hlen, w4, to_do)
	         else  (* some length problem *)
	          length_problem (packet, hlen, to_do)
	      end (* let *)
	     else
	      slow_process_packet (state, packet)
	  | _ =>
	     slow_process_packet (state, packet))
            handle x =>
	            Trace.print_handled (x, SOME "process_packet")

(*
	8.	function identify
*)

   fun slow_identify packet =
        let val (src, cursor) = Word16_In.unmarshal (packet, 0w0)
	    val (dest, _) = Word16_In.unmarshal (packet, cursor)
        in {src = src, dest = dest}
        end
       handle x => Trace.print_raise_again (x, SOME "unmarshal, identify")

   fun identify packet =
        (case expose_word_arrays packet of (* normal, fast case *)
	    [(byte_array, first, last)] =>
	     let val actual_index = Word.toInt first
	         val w1 = Pack32Big.subArr (byte_array, actual_index div 4)
	     in if actual_index mod 4 = 0 then
	         {src = Word16.fromLargeWord (Word32.>> (w1, 0w16)),
		  dest = Word16.fromLargeWord (Word32.andb (w1, w32xffff))}
	        else
		 slow_identify packet
	     end
	  | _ => slow_identify packet)
       handle _ => slow_identify packet

  end (* local *)

(*
	9.	function new
*)

  fun new {act, to_do,
	   address = Address {local_port, remote_port},
	   peer_checksum} =
       let val header = create_header (local_port, remote_port,
				       peer_checksum, [])
	   val ts = [Tcp_Tcb.Timestamp {send_time = zero32, echo = zero32}]
           val header_ts = create_header (local_port, remote_port,
					  peer_checksum, ts)
	   val (empty_packet, _) = Outgoing.split (header, min_hlen)
	   val (empty_packet_ts, _) = Outgoing.split (header_ts, ts_hlen)
	   val add = B.Checksum.one_s_add
	   fun checksum_packet p =
	        let val st = B.Checksum.initial_state
	            val fold = Outgoing.fold (p, B.Checksum.check_partial, st)
		in B.Checksum.complete_partial fold
		end
       in Word8_Out.marshal (empty_packet, ack_flag) flag_index;
          Word16_Out.marshal (empty_packet, zero_check) cks_index;
	  Word8_Out.marshal (empty_packet_ts, ack_flag) flag_index;
          Word16_Out.marshal (empty_packet_ts, zero_check) cks_index;
	  T {act = act,
	     local_port = local_port, remote_port = remote_port,
	     peer_checksum = peer_checksum,
	     header_template = header, empty_packet = empty_packet,
	     empty_checksum = add (checksum_packet empty_packet,
				   peer_checksum empty_packet),
	     header_template_with_ts_option = header_ts,
	     empty_packet_with_ts_option = empty_packet_ts,
	     empty_with_ts_checksum = add (checksum_packet empty_packet_ts,
					   peer_checksum empty_packet_ts),
	     to_do = to_do}
       end

 end (* struct *)

