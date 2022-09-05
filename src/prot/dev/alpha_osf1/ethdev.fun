(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edoardo Biagioni (esb@cs.cmu.edu)
	Ken Cline    (Ken.Cline@cs.cmu.edu)
	Nick Haines  (Nick.Haines@cs.cmu.edu)
	Brian Milnes (Brian.Milnes@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

		i.	Abstract

	ethdev.fun: an implementation of a general device signature to
	send and receive binary ethernet packets through the OSF1
	packetfilter pseudo-device.



		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor Ethernet_Device
	2.	structure Trace
	3.	internal functions raise_fun and handle_fun
	4.	structure External
	5.	functor types, exceptions and state
	6.	makestrings
	7.	management of read buffers
	8.	internal function receive_timeout
	9.	function send
	10.	function session
	11.	function set_filter
	12.	function clear_filter

*)

(*

		iii.	RCS Log

$Log: ethdev.fun,v $
Revision 1.28  1996/04/18  21:27:36  cline
converted hash from int to word

Revision 1.27  1996/04/08  19:26:44  derby
Added code to create the buffers uninitialized.

Revision 1.26  1996/03/04  20:47:53  derby
Modified the send to use writev instead of write.

Revision 1.25  1996/02/15  19:04:39  esb
fixed debug_packet to print the correct length.

Revision 1.24  1996/01/19  23:04:33  esb
adapted to the new wordarray signature.

Revision 1.23  1996/01/15  18:00:49  cline
removed FoxWord

Revision 1.22  1995/11/12  16:38:45  esb
adapted to new Word_Array.

Revision 1.21  1995/11/08  23:10:27  cline
*** empty log message ***

Revision 1.20  1995/10/17  22:32:46  esb
added some interlocks to insure proper shutdown.

Revision 1.19  1995/10/02  20:47:15  cline
check for send on closed device

Revision 1.18  1995/09/29  19:38:57  cline
fixed tracing bug

Revision 1.17  1995/09/25  15:20:41  esb
handler no longer prints exceptions named "Receive".

Revision 1.16  1995/09/18  19:29:17  esb
first running version.

Revision 1.15  1995/03/12  17:53:08  esb
adapted to new trace.sig.

Revision 1.14  1995/03/10  03:48:37  esb
adapted to new vendor.sig.

Revision 1.13  1995/03/07  23:55:37  esb
updated tracing.

Revision 1.12  1995/02/21  15:46:58  esb
fixed a minor bug.

Revision 1.11  1995/02/04  20:39:46  robby
updated to 107

Revision 1.10  1995/01/18  21:02:34  esb
adapted to new coro.sig fork_limited_time.

Revision 1.9  1995/01/16  23:48:37  esb
removed obsolete functor parameters.

Revision 1.8  1995/01/14  02:31:41  esb
adapted to new filter interface.

Revision 1.7  1995/01/06  01:35:21  esb
at initialization we now set a permissive filter for all messages.

Revision 1.6  1994/12/21  20:36:49  milnes
Updated for duplicate addressing.

Revision 1.5  1994/11/22  13:58:38  milnes
Removed addressing functor arguments.

Revision 1.4  1994/11/11  18:06:15  esb
changed the functor parameters to Debug_Trace_Printer.

Revision 1.3  1994/11/10  16:06:43  milnes
Updated for the tcpipeth/addressing scheme.

Revision 1.2  1994/11/07  21:34:33  cline
use V.Print

Revision 1.1  1994/10/20  17:57:49  cline
Initial revision


*)

(*
	1.	functor Ethernet_Device
*)

functor Ethernet_Device (structure Packet_Filter: PACKET_FILTER
			 structure B: FOX_BASIS
			   sharing type Word_Array.T = Packet_Filter.filter
			 val debug_level: int ref option): RAW_DEVICE =
 struct

(*
	2.	structure Trace
*)

  structure Trace = Trace (structure V = B.V
			   val debug_level = debug_level
			   val module_name = "ethdev.fun"
			   val makestring = fn _ => NONE)
  val do_prints = Trace.debug_on ()
  val do_traces = Trace.trace_on ()

(*
	3.	internal functions raise_fun and handle_fun

	Defined so we can consistently handle and report exceptions.
*)

  fun raise_fun (s, x) = Trace.print_raise (x, SOME s)

  fun handle_fun (x, s, f) =
       (if B.V.Control.exnName x <> "Receive" orelse Trace.trace_on () then
	 Trace.print_handled (x, SOME s)
	else ();
        f ())

(*
		4.	structure External
*)

  structure External = Protocol_External (structure B = B
					  val debug_level = debug_level)

(*
	5.	functor types, exceptions and state
*)

  exception Session_Already_Open

  structure Setup =
   struct
    type T = string
    fun makestring interface_name = "interface: " ^ interface_name
    fun equal (n0: T, n1) = (n0 = n1)
    fun hash _ = 0w0
   end (* struct *)

  type session =
        {send: External.T -> unit,
         local_address: Word_Array.T,
         packets_sent: unit -> Word64.word,
         packets_received: unit -> Word64.word,
         read_timeouts: unit -> Word64.word,
         failed_sends: unit -> Word64.word,
         packets_rejected: unit -> Word64.word}

(*
		6.	makestrings
*)

  fun format (NONE, length) = format (SOME (length - 14), length)
    | format (SOME bytes, length) =
       let val data_bytes = max (0, Integer.min (bytes, length - 14))
       in B.Format.byte_base := B.Format.Hex;
          [B.Format.String "\nTo    = ",
	   B.Format.Bytes 6,
	   B.Format.String "\nFrom  = ",
	   B.Format.Bytes 6,
	   B.Format.String "\nproto = ",
	   B.Format.Bytes 2,
	   B.Format.String "\ndata  = ",
	   B.Format.Bytes data_bytes,
	   B.Format.String (if bytes = length - 14 then "\n" else "...\n")]
       end (* let *)

  fun makestring_packet (message, bytes) =
       let val length = External.size message
	   val data = External.sub (message, {start = 0w0, length = length})
       in B.V.String.concat (B.Format.wordarray (format (bytes,
							 Word.toInt length),
						 data))
       end (* let *)

  fun print_header (data, len, s) =
       let val from8 = Word_Array.from8
	   val dest = Word_Array.W8.U_Big.R.seek (data, len - 0w6)
	   val source = Word_Array.W8.U_Big.R.seek
	                 (Word_Array.W8.U_Big.F.seek (data, 0w6), len - 0w12)
	   val proto = Word_Array.W8.U_Big.R.seek
	                 (Word_Array.W8.U_Big.F.seek (data, 0w12), len - 0w14)
       in Trace.local_print (s ^ " " ^
			     B.Format.makestring (from8 source) ^ "->" ^
			     B.Format.makestring (from8 dest) ^ ";" ^
			     B.Format.makestring (from8 proto) ^ "(" ^
			     Word.toString len ^ ")")
       end

  fun debug_print_packet (data, length, s) =
       if Word_Array.W8.U_Big.F.length data < 0w14 then
        Trace.local_print ("error in debug_print (" ^ s ^ "), array length " ^
			   Word.toString (Word_Array.W8.U_Big.F.length data) ^
			   " minimum is 14.")
       else
	let val banner = "\nethdev.fun: " ^ s ^ " packet"
	    val format = [B.Format.String banner,
			  B.Format.String "\nTo    = ",
			  B.Format.Bytes 6,
			  B.Format.String "\nFrom  = ",
			  B.Format.Bytes 6,
			  B.Format.String "\nproto = ",
			  B.Format.Bytes 2,
			  B.Format.String "\nlength = ",
			  B.Format.String (Integer.toString length),
			  B.Format.String "\ndata  = ",
			  B.Format.Bytes (min (45, length - 14)),
			  B.Format.String "...\n"]
	    fun byte_headers B.Format.Hex = "x"
	      | byte_headers B.Format.Binary = "b"
	      | byte_headers B.Format.Decimal = ""
	    val old_byte_headers = ! B.Format.byte_header
	    val _ = B.Format.byte_header := byte_headers
	    val list = B.Format.wordarray (format, Word_Array.from8 data)
	in Trace.local_print (B.V.List.fold op^ list "");
	   B.Format.byte_header := old_byte_headers
	end

   fun trace_packet (data, s) =
        print_header (data, Word_Array.W8.U_Big.F.length data, s)
   fun debug_packet (data, s) =
        debug_print_packet (data,
			    Word.toInt (Word_Array.W8.U_Big.F.length data), s)
   val log_packet = if do_prints then debug_packet
                    else if do_traces then trace_packet
		    else fn _ => ()

(*
	7.	management of read buffers

	Buffers are aligned on a word+2 boundary, to make the ethernet
	data aligned on a word boundary.
*)

  functor Buffers (val buffer_size: int
		   val max_buffers: int) =
   struct
    local
     structure W = Word_Array.W8.U_Big.F
     val zero = Word8.fromInt 0
     val real_buffer_size = Word.fromInt buffer_size + 0w2
     fun new_buffer () =
          Word_Array.from8 (W.seek (W.create_uninitialized real_buffer_size, 0w2))
     val cache = ref [new_buffer ()]
     val present = ref 1
    in

     fun get () =
          case ! cache of
	     [] => new_buffer ()
	   | head :: rest =>
	      (cache := rest;
	       present := ! present - 1;
	       head)

     fun recycle b =	(* also add a buffer if ! present < max_buffer. *)
          (cache := b :: (! cache);
	   present := ! present + 1;
	   if ! present < max_buffers then
	    (cache := (new_buffer ()) :: (! cache);
	     present := ! present + 1)
	   else ())

    end (* local *)
   end (* struct *)

(*
	8.	internal function receive_timeout

	Receive_timeout is a polling function for the packetfilter
	pseudo-device.  We install it in the scheduler, and it is
	called whenever the scheduler has no active threads and
	whenever the scheduler switches threads. The numerical
	parameter is the number of milli-seconds (ms) that the
	function should take.
*)

  val one64 = Word64.fromInt 1

  local

   val buffer_size = 1650		(* don't know the exact lower limit:
					   should be 1546 for ethernet, but
					   there are mach headers and such *)

   (* max buffers is the number of buffers we try to build up to
      during idle times. If you make it too large you needlessly
      take up memory space and g.c. too often; if you make it too
      small, it will take longer (on average) to receive a packet
      that is ready to be received. *)
   val max_buffers = 10

   structure Buffers = Buffers (val buffer_size = buffer_size
				val max_buffers = max_buffers)

  in (* local *)

   fun receive_timeout (fd, self, handler, packets_received,
			read_timeouts) ms =
        (B.Scheduler.fork_limited_time (! self);
	 if Packet_Filter.select (fd, ms) then
	  let val buffer = Buffers.get ()
	      val read_args = (fd, buffer, buffer_size, 0)
	      val bytes_read = Packet_Filter.readi read_args
	  in log_packet (Word_Array.to8 buffer, "received");
	     packets_received := Word64.+ (! packets_received, one64);
	     (((! handler) (External.new buffer))
	      handle x => handle_fun (x, "packet handler", fn _ => ()))
	  end
	 else read_timeouts := Word64.+ (! read_timeouts, one64))
  end (* local *)

(*
	9.	function send
*)

  fun send (fd, closed, packets_sent, failed_sends) packet =
     let val bufferList = rev (External.fold (packet, op :: ,  []))
     in (( (*log_packet (Word_Array.to8 buffer, "sending ");*)
          if !closed then
             Trace.local_print "ignoring attempt to send on closed device"
          else
             (Packet_Filter.writev (fd, bufferList);
             packets_sent := Word64.+ (! packets_sent, one64);
             ()))
         handle x =>
            (Trace.print_handled (x, SOME "send");
             failed_sends := Word64.+ (! failed_sends, one64)))
     end

(*
		10.	function session
*)

  val zero64 = Word64.fromInt 0
  datatype 'a result = Result of 'a | Exception of exn
  fun session (interface_name, f) =
       case ((let val fd = Packet_Filter.pfopen interface_name
	          val addr = Packet_Filter.get_ethernet_address fd
		  val all_filter = B.Filter.True
		  val compiled_filter = B.Filter.make all_filter
(*
		  val _ = Packet_Filter.set_filter (fd, compiled_filter)
*)
		  fun dummy_receive _ = ()
		  fun dummy_handler _ = ()
		  val sent = ref zero64
		  val received = ref zero64
		  val failed = ref zero64
		  val timeouts = ref zero64
		  val closed = ref false
		  val session_arg = {send = send (fd, closed, sent, failed),
				     local_address = addr,
				     packets_sent = fn _ => ! sent,
				     packets_received = fn _ => ! received,
				     read_timeouts = fn _ => ! timeouts,
				     failed_sends = fn _ => ! failed,
				     packets_rejected = fn _ => zero64}
		  val (session_handler, data_handler) = f session_arg
		  val handler: (External.T -> unit) ref = ref data_handler
(* The value "receive" is initialized to "ref dummy_receive" since
immediately initializing it to its final value has problems of
recursive definition (ML supports recursive definition, but only for
functions, not refs).  *)
		  val receive: (int -> unit) ref = ref dummy_receive
		  val _ = receive := receive_timeout (fd, receive, handler,
						      received, timeouts);
		  val _ = B.Scheduler.fork_limited_time (! receive)
		  val result = ((Result (session_handler ()))
				handle x => Exception x)
(* reset "receive" to keep from forking in the limited queue, and reset
   "handler" and "closed" to prevent reception and sending by the higher
   protocol. *)
	      in receive := dummy_receive;
	         handler := dummy_handler;
		 closed := true;
	         Packet_Filter.close fd;
		 result
	      end)
	       handle x =>
		       Trace.print_raise_again (x, SOME "session")) of
	  Result x => x
	| Exception x => raise x

(* do not delete this comment unless you have implemented filters.

(*
		11.	function set_filter
*)

  type filter = B.Filter.filter

  fun set_filter filter =
       case ! state of
	  NONE =>
	   Trace.print_raise ("protocol not initialized", Protocol_Finalized)
	| SOME (Interface {pf, ...}) =>
	   let val compiled_filter = B.Filter.make filter
	       fun raise_init () =
		    raise Initialization_Failed "error setting device filter"
	   in Trace.debug_print (fn _ => "set_filter:  [" ^
				 B.Filter.makestring filter ^ "]");
	      ((Packet_Filter.set_filter (pf, compiled_filter))
	       handle x =>
	               handle_fun (x, "device_set_filter", raise_init))
	   end (* let *)

(*
		12.	function clear_filter
*)

  fun clear_filter () = set_filter B.Filter.True
*)

 end (* struct *)
