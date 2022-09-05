(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edoardo Biagioni (esb@cs.cmu.edu)
	Nick Haines  (Nick.Haines@cs.cmu.edu)
	Brian Milnes (Brian.Milnes@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

		i.	Abstract

	ethdev.fun: an implementation of a general device signature to
	send and receive binary ethernet packets through Mach messages.



		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor Ethernet_Device
	2.	internal functions raise_fun and handle_fun
	3.	Interface
	4.	functor types, exceptions and state
	5.	makestring functions
	6.	internal function u_mach_msg
	7.	internal mach options functions
	8.	internal function close_port
	9.	internal function get_write_port
	10.	internal function get_ethernet_address
	11.	internal function get_read_port
	12.	management of read buffers
	13.	internal function receive_timeout
	14.	function set_filter
	15.	function clear_filter
	16.	function initialize
	17.	function finalize
	18.	functions connect, *_passive, close, abort
	19.	device-specific functions
	20.	function send

*)

(*

		iii.	RCS Log

$Log: ethdev.fun,v $
Revision 1.12  1995/03/12  17:53:24  esb
adapted to new trace.sig.

Revision 1.11  1995/03/10  03:48:45  esb
adapted to new vendor.sig.

Revision 1.10  1995/03/07  23:53:58  esb
updated to 1.07.

Revision 1.9  1995/02/04  20:39:49  robby
updated to 107

Revision 1.8  1995/01/18  21:02:53  esb
adapted to new coro.sig fork_limited_time.

Revision 1.7  1995/01/16  23:48:50  esb
removed obsolete functor parameters.

Revision 1.6  1995/01/14  02:33:00  esb
adapted to new filter interface.

Revision 1.5  1995/01/06  01:35:51  esb
at initialization we now set a permissive filter for all messages.

Revision 1.4  1994/12/21  20:36:49  milnes
Updated for new shadowed addressing, but it produces terrible performance
problems when the duplicate filters are installed.

Revision 1.3  1994/11/11  18:07:20  esb
changed the functor parameters for Debug_Trace_Printer.

Revision 1.2  1994/11/10  16:06:43  milnes
Updated for the tcpipeth/addressing scheme.

Revision 1.1  1994/10/20  17:56:54  cline
Initial revision

Revision 1.70  1994/09/30  17:02:17  esb
now uses B.V.create_buffer instead of B.Create.create_buffer

Revision 1.69  1994/09/27  16:38:56  milnes
Removed a variable that I had left lying around.

Revision 1.68  1994/09/12  18:19:29  milnes
Added a functor application time optional ip address that is added to the
packet filter.

Revision 1.67  1994/08/24  22:25:24  esb
changed to use arrays instead of buffers in receiving.

Revision 1.66  1994/08/12  06:17:32  esb
bug fixes, and got the construction of mach_msg parameters out of send path.

Revision 1.65  1994/08/02  20:25:36  esb
adapted to new protocol signature.

Revision 1.64  1994/07/04  21:34:16  esb
adapted to Copy/Create split.

Revision 1.63  1994/07/01  02:34:13  danwang
Moved control structures into Fox_Basis.

Revision 1.62  1994/06/16  16:45:05  danwang
Updated to use functorized Fox_Basis

Revision 1.61  1994/06/05  18:44:23  milnes
Added arp filter support.

Revision 1.60  1994/05/23  14:04:12  milnes
Added print functions.

Revision 1.59  1994/04/26  17:57:39  esb
adapted to new COROUTINE signature.

Revision 1.58  1994/04/08  13:59:35  esb
changed to non-zero timeout to work around a Mach scheduling bug.

Revision 1.57  1994/04/06  23:20:35  esb
adapted to new receive_packet interface, fixed the packets_rejected counter.

Revision 1.56  94/03/30  15:36:23  esb
many optimizations and comment changes.

Revision 1.55  1994/03/29  17:44:40  milnes
Bounds checked format call.

Revision 1.54  1994/03/10  20:14:35  esb
introduced Copy.create_buffer.

Revision 1.53  94/03/07  15:12:52  esb
major clean up, eliminating functor parameters and introducing send-buffer
recycling.

Revision 1.52  94/02/25  18:35:12  milnes
Updated timing and moved to safe array operations.

Revision 1.51  1994/02/18  14:33:35  milnes
Extended the timing calls and made the fast path timing calls non-cumulative.

Revision 1.50  1994/02/17  17:10:27  milnes
Checked out, reinstalled old, rechecked in to repair the afs file move problems that confused rcs.

Revision 1.48  1994/02/04  11:15:44  esb
reduced the buffer size to 2000 (1550 is too small).

Revision 1.47  94/01/30  18:16:34  milnes
added statistics.

Revision 1.46  1994/01/29  01:53:29  esb
fixed a bug whereby buffers were recycled more than once.

Revision 1.45  1994/01/19  21:38:50  esb
improved a comment.

Revision 1.44  1994/01/19  21:23:34  esb
fixed a bug that prevented sending maximum sized ethernet packets.
Maximum send size is 1514, but we were rounding up to words,
which caused the size to become 1516 which didn't work. It is
apparently not necessary to round up to full words.

Revision 1.43  1994/01/19  16:18:36  esb
minor changes.

Revision 1.42  1994/01/17  18:12:18  esb
interface changes.

Revision 1.41  1994/01/13  16:22:38  milnes
Added kills for low priority threads.

Revision 1.40  93/12/09  19:42:19  milnes
Updated to handle scheduler, proto.sig, dev.sig and timeprotocol.sig changes.

Revision 1.38  1993/11/09  22:12:25  milnes
Added an unsafe mach msg call (inline) that returns an error code
that we can check. The other unsafe one was not checking the status code.

Revision 1.37  1993/10/28  16:45:23  milnes
 Updated the error messages.

Revision 1.36  1993/10/27  18:36:31  esb
fixed a bug that caused the read to block until the next packet was received.

Revision 1.35  1993/10/26  20:07:05  esb
Fixed the bug where the buffer is reused (but left it optional).

Revision 1.34  1993/10/25  20:37:04  milnes
Well, I did it right this time.

Revision 1.33  1993/10/25  20:32:06  milnes
Removed the usage of  Byte.S.sbytes of task by pid.

Revision 1.32  1993/10/25  19:36:16  cline
removed .U from Byte[421].U

Revision 1.31  1993/10/25  17:43:35  milnes
Changed safe_msg and zero_timeout so that they are real structure components.

Revision 1.30  1993/10/14  22:36:29  esb
now filters on multiple protocols numbers rather than just one.

Revision 1.29  93/10/08  15:44:28  milnes
Tweaked a bit.

Revision 1.28  1993/09/22  19:31:56  milnes
Removed "-----------"s.

Revision 1.27  1993/09/17  16:43:19  milnes
Changed default parameters.

Revision 1.26  1993/09/13  22:15:49  cline
removed "

Revision 1.25  1993/09/02  22:18:00  esb
fixed a byte ordering problem.

Revision 1.24  1993/09/02  15:53:06  esb
cleaned up a lot.

Revision 1.23  1993/08/26  17:21:05  esb
fixed a minor bug

Revision 1.22  1993/08/24  21:08:44  esb
fixed a byte-ordering bug in the filter and improved the trace statements.

Revision 1.21  1993/07/30  14:19:31  esb
fixed send_packet to correctly use device_write_request, and fixed a
lot of finalization stuff.

Revision 1.20  1993/07/26  12:24:42  milnes
Updated to use a high priority filter so that the BSD server would
not also get our packets (with a different type field). Also had to
install a bogus filter before we close the ethernet or Mach did not
remove the old packet filter and we hung.

Revision 1.19  1993/07/22  21:05:44  nickh
*** empty log message ***

Revision 1.18  1993/07/22  21:03:02  nickh
Added do_if_debug.

Revision 1.17  1993/07/22  18:43:22  nickh
Adjusted offsets for sub and update to use byte offsets.

Revision 1.16  1993/07/22  18:28:21  esb
put in zero timeouts, device_write_request.

Revision 1.15  1993/07/12  14:40:58  esb
changed outgoing_message from bytearray to Send_Packet.T

Revision 1.14  1993/07/11  02:16:16  esb
got it to work with no data copying, only copying of ethernet header

Revision 1.13  1993/07/10  01:22:02  nickh
replaced MachIPC with Local_Mach and added functor arguments to select
either MachIPC.mach_msg or MachIPC_Unsafe.mach_msg
(checked in by esb)

Revision 1.12  1993/07/07  20:44:41  nickh
Ethernet_Device should take a MachIPC argument, both for software
engineering reasons and to enable us to use MachIPC_Unsafe, which avoids
the multiple-kernel-threads approach to the mach_msg implementation.

Revision 1.11  1993/07/07  18:46:46  milnes
Updated the printing and statistics collected in ethdev.tim.

Revision 1.10  1993/07/01  18:10:19  nickh
Change receive_timeout: the data-copying loop no longer has an expensive
pattern match, and the data is not copied at all in the case of a timeout.

Revision 1.9  1993/06/30  20:34:55  nickh
Fixed the handler for timeouts. It had a broken pattern which didn't
do what the author intended.

Revision 1.8  1993/06/30  02:11:43  esb
fixed send_data so device_write_inband is only used for buffer
sizes of 128 bytes or less (66 bytes or less of ethernet payload).

*)

(*
	1.	functor Ethernet_Device
*)

functor Ethernet_Device (val device_name: string
			 structure Device: DEVICE
			 structure Local_Mach: MACHIPC
			 structure Mach_Port: MACH_PORT
			 structure B: FOX_BASIS
			 sharing type Device.buf = Local_Mach.buf
				    = ByteArray.bytearray
			     and type Device.word = Word.word
			            = Local_Mach.word = Device.port
				    = Local_Mach.port = Mach_Port.port
				    = Mach_Port.word = FoxWord32.word
			     and type Device.msg_field_name
			            = Local_Mach.msg_field_name
			 val task_by_pid: FoxWord32.word -> Mach_Port.port
			 val high_priority_filter: bool
			 val debug_level: int ref option): DEVICE_PROTOCOL =
 struct

  structure Trace = Trace (structure V = B.V
			   val debug_level = debug_level
			   val module_name = "ethdev.fun")

  val local_print = Trace.local_print
  val debug_print = Trace.debug_print

  val n4u0 = SW.n32 "0"
  val n4u1 = SW.n32 "1"
  val n4u2 = SW.n32 "2"
  val n4u8 = SW.n32 "8"
  val n4u14 = SW.n32 "14"
  val n4u16 = SW.n32 "16"
  val n4u25 = SW.n32 "25"
  val n4u28 = SW.n32 "28"
  val n4u100 = SW.n32 "100"
  val n4u2802 = SW.n32 "2802"
  val n4u2803 = SW.n32 "2803"
  val n4ux10004003 = SW.n32 "0x10004003"
  val n4ux10004004 = SW.n32 "0x10004004"
  val n4uxFFFFFFFE = SW.n32 "0xfffffffe"

  val lower_mask = SW.n32"0x0000ffff"
  val upper_mask = SW.n32"0xffff0000"

  fun word32_to_int_pair w = 
       (FoxWord32.wordToInt (FoxWord32.rshift
			     (FoxWord32.andb (upper_mask, w), 16)),
	FoxWord32.wordToInt (FoxWord32.andb (lower_mask,w)))
  fun int_pair_to_word32 (lower, upper) = 
       (FoxWord32.andb (FoxWord32.lshift (FoxWord32.intToWord upper, 16),
			FoxWord32.intToWord lower))

(*	the structure Device provides various calls. The ones we use are:

	    device_open, device_get_status, device_set_filter, device_close

	We have inlined the call to to device_write (and changed it to
	device_write_request). In principle we could inline these
	other calls also, but they are not performance-critical so we
	have chosen not to do so.
*)

(*
	2.	internal functions raise_fun and handle_fun

	Defined so we can consistently handle and report exceptions.
*)

  fun raise_fun (s, x) =
       (local_print s;
	raise x)

  fun handle_fun (x, s, f) =
   ((case x of
        Mach_Port.Mach_Port w => (* Mach_Port error, print the error code. *)
         local_print (s ^ " error, mach code " ^ FoxMakestring.word32 w)
      | Local_Mach.Mach_Error e =>
         local_print (s ^ " error, mach error " ^ e)
      | Device.Device n =>
         local_print (s ^ " error, Device.device " ^ FoxMakestring.word32 n)
      | x =>
         local_print (s ^ " error, handling exception " ^ System.exn_name x));
    f ())

(*
		3.	Interface


	The state = Interface {...} type holds the state of the
	ethernet device.

	Address is the address, a bytearray of size 6.

	Write_port is the port through which which we send send
	control operations and data to the ethernet controller, using
	mach_msg.

	Read_port is a filtered read port from which we read packets,
	using mach_msg.

	Receive_handler is the closure to which we hand packets.

	Read_attempts is the number of times the device attempted
	to read from the mach port.
	Read_timeouts is the number of timed-out attempts to read the
	device port via mach msg. Read_attempts includes read_timeouts.
	The other statistics should be self-explanatory.

	Arp_filter is the protocol address for which we are serving ARPs,
	if any.

	Count is a reference count: the number of initialize calls -
	number of finalize calls, but no less than zero.

*)

  type incoming = B.Dyn_Array.T
  type outgoing = B.Dyn_Array.T

  datatype state = Interface of {address: ByteArray.bytearray,
				 write_port: Local_Mach.port,
				 read_port: Local_Mach.port,
				 receive_handler: (incoming -> unit) ref,
				 read_attempts: int ref,
				 read_timeouts: int ref,
				 packets_sent: int ref,
				 packets_received: int ref,
				 packets_rejected: int ref,
				 mach_msg_returned: int ref,
				 count: int ref}

  fun makestring_ethernet addr_buf =
       let val b0_index = if FoxWord32.endian() = FoxWord32.Big then 0 else 3
	   val b1_index = if FoxWord32.endian() = FoxWord32.Big then 1 else 2
	   val b2_index = if FoxWord32.endian() = FoxWord32.Big then 2 else 1
	   val b3_index = if FoxWord32.endian() = FoxWord32.Big then 3 else 0
	   val b4_index = if FoxWord32.endian() = FoxWord32.Big then 4 else 7
	   val b5_index = if FoxWord32.endian() = FoxWord32.Big then 5 else 6
	   val b0 = FoxWord8.sub (addr_buf, b0_index)
	   val b1 = FoxWord8.sub (addr_buf, b1_index)
	   val b2 = FoxWord8.sub (addr_buf, b2_index)
	   val b3 = FoxWord8.sub (addr_buf, b3_index)
	   val b4 = FoxWord8.sub (addr_buf, b4_index)
	   val b5 = FoxWord8.sub (addr_buf, b5_index)
       in FoxMakestring.word8 b0 ^ ":" ^ FoxMakestring.word8 b1 ^ ":" ^
	  FoxMakestring.word8 b2 ^ ":" ^ FoxMakestring.word8 b3 ^ ":" ^
	  FoxMakestring.word8 b4 ^ ":" ^ FoxMakestring.word8 b5
       end

  fun discard_packet _ = ()		(* handler called if none defined *)

  (* we need a central state to reflect the state of the ethernet port:
     allocated and initialized, or not allocated and initialized. *)
  val state = ref (NONE: state option)

(*
	4.	functor types, exceptions and state
*)

  type address = unit
  type status = unit
  type connection = unit
  type allocation = int

  datatype handler = Handler of connection -> ((incoming -> unit)
					       * (status -> unit))

  exception Initialization_Failed of string
  exception Protocol_Finalized
  exception Connection_Closed of connection * string
  exception Illegal_Address of string
  exception Open_Failed of string
  exception Send_Failed of string

(*
		5.	makestring functions
*)

  fun makestring_address () = "()"
  val makestring_status =  makestring_address
  val makestring_connection = makestring_address

  fun format (NONE, length) = format (SOME (length - 14), length)
    | format (SOME bytes, length) =
       let val data_bytes = max (0, B.V.Integer.min (bytes, length - 14))
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

  fun makestring_outgoing (message, bytes) =
       let val length = B.Dyn_Array.size message
	   val data = B.Dyn_Array.read message
       in 
	 B.V.String.concat
	 (B.Format.bytearray (format (bytes, length), data, 0))
       end (* let *)

  val makestring_incoming = makestring_outgoing

  fun hash_address _ = 0
  val hash_connection = hash_address
  fun equal_address _ = true
  val equal_connection = equal_address

(*
	6.	internal function u_mach_msg

	This is the simplest, fastest mach_msg we can get in ML. It
	returns a byte-4 result value in a 4-byte bytearray.
*)

   val u_mach_msg: ByteArray.bytearray * int * int * int
                 * Mach_Port.port * int * Mach_Port.port -> FoxWord32.word =
         System.Unsafe.CInterface.c_function "FoxNet" "mach_msg"

(*
	7.	internal mach options functions
*)

   fun msg_option_to_int Local_Mach.MSG_OPTION_NONE           = 0x00000000
     | msg_option_to_int Local_Mach.MSG_OPTION_SEND           = 0x00000001
     | msg_option_to_int Local_Mach.MSG_OPTION_RCV            = 0x00000002
     | msg_option_to_int Local_Mach.MSG_OPTION_SEND_TIMEOUT   = 0x00000010
     | msg_option_to_int Local_Mach.MSG_OPTION_SEND_NOTIFY    = 0x00000020
     | msg_option_to_int Local_Mach.MSG_OPTION_SEND_INTERRUPT = 0x00000040
     | msg_option_to_int Local_Mach.MSG_OPTION_SEND_CANCEL    = 0x00000080
     | msg_option_to_int Local_Mach.MSG_OPTION_RCV_TIMEOUT    = 0x00000100
     | msg_option_to_int Local_Mach.MSG_OPTION_RCV_NOTIFY     = 0x00000200
     | msg_option_to_int Local_Mach.MSG_OPTION_RCV_INTERRUPT  = 0x00000400
     | msg_option_to_int Local_Mach.MSG_OPTION_RCV_LARGE      = 0x00000800

   fun encode_options options =
        let fun f (opt, i) = Bits.orb ((msg_option_to_int opt), i)
	in B.V.List.fold f options 0
	end

(*
	8.	internal function close_port

	Close a port, and print an error if something goes wrong, but don't
	raise an exception.

*)

  fun close_port port =
       let val self_task = Local_Mach.task_self ()
 (* mach_portUser.sig: val mach_port_destroy: port * port -> unit *)
       in (Mach_Port.mach_port_destroy (self_task, port))
           handle x => handle_fun (x, "mach_port_destroy", (fn () => ()))
       end

(*
	9.	internal function get_write_port

	Get_write_port obtains a write port to the ethernet device driver.
	It calls:
	  device_open to open the device
	  device_get_status to make sure the open was successful
*)

  fun get_write_port (name, wp) =
       let fun raise_init name () =
	        raise_fun ("initialization fails, probably not rootl",
		           Initialization_Failed (name ^ " raised Device"))
  (* deviceUser.sig: val device_open: port * word * string -> port *)
	   val write_port = Device.device_open (wp, n4u0, name)
	                     handle x => handle_fun (x, "device_open",
						     raise_init "device_open")
  (* device_get_status returns 7 ints:
     (from device/net_status.h, Revision 2.8  92/02/25  15:34:48  elf):
         struct net_status {
          int min_packet_size;        /* minimum size, including header */
          int max_packet_size;        /* maximum size, including header */
          int header_format;          /* format of network header */
          int header_size;            /* size of network header */
          int address_size;           /* size of network address */
          int flags;                  /* interface status */
          int mapped_size;            /* if mappable, virtual mem needed */
        };
        #define NET_STATUS_COUNT        (sizeof(struct net_status)/sizeof(int))
   *)
           val status_length = n4u28 (*   7 * 4   *)
(* /afs/cs/project/mach3/alpha/release/pmax_mach/include/device/net_status.h,
  Revision 2.8
 #define	NET_STATUS		(('n'<<16) + 1) *)
	   val NET_STATUS = FoxWord32.+ 
	     (FoxWord32.lshift (FoxWord32.intToWord (Char.ord #"n"), 16),
	      n4u1)
  (* deviceUser.sig: val device_get_status: port * word * word
                                          -> (Device.buf * int) *)
	   val (status, len) =
	        (Device.device_get_status (write_port, NET_STATUS,
					   status_length))
		handle x => handle_fun (x, "device_get_status",
					raise_init "device_get_status")
  (* device/net_status: #define HDR_ETHERNET 1 /* Eth hardware address */ *)
	   val HDR_ETHERNET = n4u1
	   val HEADER_SIZE = n4u14
  (* This is the maximum size of an ethernet packet that can
     be sent using Mach. This size includes the ethernet header,
     so the maximum size data that can be sent is 1500 bytes. *)
	   val MAX_SIZE = 1514
	   val word_size: int = 4
	   val returned_header_format = FoxWord32.sub (status, 2 * word_size)
	   val returned_header_size = FoxWord32.sub (status, 3 * word_size)
	   val returned_max_size =
	         FoxWord32.wordToInt (FoxWord32.sub (status, 1 * word_size))
	   val returned_mapped_size = FoxWord32.sub (status, 6 * word_size)
       in if returned_header_format <> HDR_ETHERNET orelse
             returned_header_size <> HEADER_SIZE orelse
	     returned_max_size > MAX_SIZE then
	   (close_port write_port;
	    raise_init "get_write_port" ())
          else if returned_mapped_size <> n4u0 then
	   (close_port write_port;
	    raise_fun ("interface already initialized",
		       Initialization_Failed "interface already initialized"))
          else
	   (debug_print (fn _ => "write port max size is " ^
			 B.V.Integer.makestring returned_max_size);
	    write_port)
       end  (* let *)

(*
	10.	internal function get_ethernet_address

	The Mach net_device knows its ethernet address and we must
	query it to find out what the network address is.

*)

  fun get_ethernet_address write_port =
       let val status_length = n4u8
(* /afs/cs/project/mach3/alpha/release/pmax_mach/include/device/net_status.h,
   Revision 2.8
   #define	NET_ADDRESS		(('n'<<16) + 2) *)
           val NET_ADDRESS = FoxWord32.+ 
	     (FoxWord32.lshift (FoxWord32.intToWord (Char.ord #"n"), 16),
	      n4u2)
           fun raise_init () =
	         raise Initialization_Failed
		         "unable to get device status for network address"
	   val (addr_buf, len) =
	         (Device.device_get_status (write_port, NET_ADDRESS,
					    status_length))
		 handle x => handle_fun (x, "device_get_status", raise_init)
(* on big- and little-endian machines, the bytes are in opposite order;
   Mach is set up to do a word-wide read of the address,
   not a byte-by-byte read. *)
	   val b0_index = if FoxWord32.endian () = FoxWord32.Big then 0 else 3
	   val b1_index = if FoxWord32.endian () = FoxWord32.Big then 1 else 2
	   val b2_index = if FoxWord32.endian () = FoxWord32.Big then 2 else 1
	   val b3_index = if FoxWord32.endian () = FoxWord32.Big then 3 else 0
	   val b4_index = if FoxWord32.endian () = FoxWord32.Big then 4 else 7
	   val b5_index = if FoxWord32.endian () = FoxWord32.Big then 5 else 6
	   val b0 = FoxWord8.sub (addr_buf, b0_index)
	   val b1 = FoxWord8.sub (addr_buf, b1_index)
	   val b2 = FoxWord8.sub (addr_buf, b2_index)
	   val b3 = FoxWord8.sub (addr_buf, b3_index)
	   val b4 = FoxWord8.sub (addr_buf, b4_index)
	   val b5 = FoxWord8.sub (addr_buf, b5_index)
	   fun makestring_addr () =
	        FoxMakestring.word8 b0 ^ ":" ^ FoxMakestring.word8 b1 ^ ":" ^
		FoxMakestring.word8 b2 ^ ":" ^ FoxMakestring.word8 b3 ^ ":" ^
		FoxMakestring.word8 b4 ^ ":" ^ FoxMakestring.word8 b5
	   fun print_address () =
	        local_print ("get_ethernet_address gives " ^
			     makestring_addr ())
	   fun get_addr 0 = b0
	     | get_addr 1 = b1
	     | get_addr 2 = b2
	     | get_addr 3 = b3
	     | get_addr 4 = b4
	     | get_addr _ = b5
	   val address = B.Create.create_fn (6, get_addr)
       in Trace.do_if_debug print_address;
	  address
       end (* let *)

(*

	11.	internal function get_read_port

	The network device has a read port from which its users collect
	its messages. In the case of ethernet, these messages can be
	reconstructed into ethernet packets. The port right is collected
	from mach and has the maximal sized queue of packets allocated (16).

*)

(* From /afs/cs/project/mach3/alpha/release/pmax_mach/include/mach/message.h
   Revision 2.14
 #define MACH_MSG_TYPE_MAKE_SEND	20	/* Must hold receive rights */
   From /afs/cs/project/mach3/alpha/release/pmax_mach/include/mach/port.h
   Revision 2.8
 #define MACH_PORT_QLIMIT_MAX		((mach_port_msgcount_t) 16)
 #define MACH_PORT_RIGHT_RECEIVE	((mach_port_right_t) 1)
*)

  val MACH_MSG_TYPE_MAKE_SEND = 20
  val MACH_PORT_QLIMIT_MAX = n4u16
  val MACH_PORT_RIGHT_RECEIVE = n4u1

  fun get_read_port () =
       let val task_self = Local_Mach.task_self ()
   (* mach_portUser.sig: val mach_port_allocate: port * word -> port *)
           val rp = Mach_Port.mach_port_allocate (task_self,
						  MACH_PORT_RIGHT_RECEIVE)
   (* port_set_qlimit: task * port * word -> unit *)
   (* set the Queue limit as large as possible to allow Mach to buffer
      as many messages as possible. *)
       in Mach_Port.mach_port_set_qlimit (task_self, rp,
					   MACH_PORT_QLIMIT_MAX);
   (* return the port *)
	  rp
       end

(*
	12.	management of read buffers
*)

  functor Buffers (val buffer_size: int
		   val max_buffers: int) =
   struct
    local
     (* fun new_buffer () = B.Create.create buffer_size *)
     fun new_buffer () = B.Create.create buffer_size
     val cache = ref [new_buffer ()]
     val present = ref 0
    in

     fun get () =
          case ! cache of
	     [] => new_buffer ()
	   | head :: rest =>
	      (cache := rest;
	       dec present;
	       head)

     fun recycle b =	(* also add a buffer if ! present < max_buffer. *)
          (cache := b :: (! cache);
	   inc present;
	   if ! present < max_buffers then
	    (cache := (new_buffer ()) :: (! cache);
	     inc present)
	   else ())

    end (* local *)
   end (* struct *)

(*
	13.	internal function receive_timeout

	Receive_timeout is a polling function for the mach port.
	We install it in the scheduler, and it is called whenever
	the scheduler has no active threads and whenever the
	scheduler switches threads. The numerical parameter is the
	number of milli-seconds (ms) that the function should take.
*)

  local

   val buffer_size = 1650		(* don't know the exact lower limit *)

   (* max buffers is the number of buffers we try to build up to
      during idle times. If you make it too large you needlessly
      take up memory space and g.c. too often; if you make it too
      small, it will take longer (on average) to receive a packet
      that is ready to be received. *)
   val max_buffers = 1

   structure Buffers = Buffers (val buffer_size = buffer_size
				val max_buffers = max_buffers)

   val options = encode_options [Local_Mach.MSG_OPTION_RCV,
				 Local_Mach.MSG_OPTION_RCV_TIMEOUT]

   fun print_header (data, header_off, len) =
        let fun bytes n = 
	  (FoxWord8.wordToInt (FoxWord8.sub (data, header_off + n)),
	   FoxWord8.wordToInt (FoxWord8.sub (data, header_off + n + 1)),
	   FoxWord8.wordToInt (FoxWord8.sub (data, header_off + n + 2)),
	   FoxWord8.wordToInt (FoxWord8.sub (data, header_off + n + 3)))
            val (dbg00, dbg01, dbg02, dbg03) = bytes 0
	    val (dbg10, dbg11, dbg12, dbg13) = bytes 4
	    val (dbg20, dbg21, dbg22, dbg23) = bytes 8
	    val (dbg30, dbg31, _, _) = bytes 12
            val m = B.V.Integer.makestring
        in local_print (m dbg12 ^ ":" ^ m dbg13 ^ ":" ^
			m dbg20 ^ ":" ^ m dbg21 ^ ":" ^ m dbg22 ^ ":" ^
			m dbg23 ^ "->" ^
			m dbg00 ^ ":" ^ m dbg01 ^ ":" ^ m dbg02 ^ ":" ^
			m dbg03 ^ ":" ^ m dbg10 ^ ":" ^ m dbg11 ^ ";" ^
			m dbg30 ^ "." ^ m dbg31 ^
			"(" ^ m len ^ ")")
         end

(*
	The message returned by Mach has the following format:
	0..26: 26 bytes of header and other information
	26..27: 2-byte offset pointer to the Mach "data"
	28..29: 2-byte offset pointer to the Ethernet header
	30+: other stuff

	The header is 16 words, namely the ethernet header followed
	by two bytes (for alignment).

	Within the Mach "data", we have:
	the header, at header offset (16 bytes: 14 for the ethernet
	                              header, 2 for alignment)
	the data size (2 bytes), at data offset + header offset + 2
	the data, at data offset + header offset + 8

	reconstruct_packet copies the ethernet header so it is
	adjacent to the data, except for two bytes which we leave
	between the ethernet header and the data so the data can still
	be word-aligned.

	reconstruct_packet expects the size of the data to be
	buffer_size, and may break if called with other packet sizes.
*)

   val data_off_index = 26	(* position of pointer to data *)
   val header_off = 28	(* position of Eth header *)
   val header_size = 16	(* size of Eth header *)
   val mach_descriptor_size = 4 (* size of Mach descriptor *)
   val constant_offset = header_off + mach_descriptor_size + 4
   val ethernet_max_size = 1516

   fun sub_mod_4096 (data, index) = FoxWord16.wordToInt 
        (FoxWord16.andb (FoxWord16.sub (data, index), SW.n16 "0xfff"))

(* if the mach data lenght is desired, it can be computed as follows:
	     (* get the data size, a pointer to which is in the descriptor. *)
	     val data_size_off = data_off + header_off + 2
	     val mach_data_size = sub_mod_4096 (data, data_size_off)
   This is the size of the Mach data.
*)
   fun reconstruct_packet data =
        (let (* get the "Mach data" (packet + descriptor) pointer. *)
	     val data_off = sub_mod_4096 (data, data_off_index)
	     val offset = constant_offset + data_off
	     val header_0 = FoxWord32.sub (data, header_off + 0)
	     val header_1 = FoxWord32.sub (data, header_off + 4)
	     val header_2 = FoxWord32.sub (data, header_off + 8)
	     val header_3 = FoxWord32.sub (data, header_off + 12)
	     (* this is the data start after the copy is complete. *)
	     val data_start = offset - header_size
         in Trace.do_if_debug (fn _ => print_header (data, header_off, 0));
	    FoxWord32.update (data, offset - 16, header_0);
	    FoxWord32.update (data, offset - 12, header_1);
	    FoxWord32.update (data, offset -  8, header_2);
	    FoxWord32.update (data, offset -  4, header_3);
	    SOME (B.Dyn_Array.tail (B.Dyn_Array.init data, data_start))
         end)
          handle x => handle_fun (x, "reconstruct_packet", fn () => NONE)

  in (* local *)

   fun receive_timeout (rp, id) ms =
        case ! state of
           NONE => () (* I am a dead receive_timeout thread for a
		       finalized ethernet thread, so do nothing. *)
	 | SOME (Interface {read_attempts, read_timeouts,
			    receive_handler, count,
			    packets_received, packets_rejected,
			    mach_msg_returned, ...}) =>
	   if count <> id then ()	(* state is not my state, terminate. *)
	   else
	    let val buffer = Buffers.get ()
	        val mach_args = (buffer, options, 0, buffer_size, rp,
				 ms, Local_Mach.port_null)
		val fork = receive_timeout (rp, id)
	   in read_attempts := ! read_attempts + 1;
	   (* Restart the low priority thread that reads from the device. *)
	      B.Scheduler.fork_limited_time fork;
	      (case word32_to_int_pair (u_mach_msg mach_args) of
(* mach return codes are defined in
 /afs/cs.cmu.edu/project/mach3/latest/release/pmax_mach/include/mach/message.h
*)
		  (0, 0) =>		(* MACH_MSG_SUCCESS: Valid packet. *)
		   (mach_msg_returned := ! mach_msg_returned + 1;
		    (case reconstruct_packet buffer of
		        SOME packet =>
			 (packets_received := ! packets_received + 1;
			  (((! receive_handler) packet)
			   handle x => handle_fun (x, "packet handler",
						   fn _ => ())))
		      | _ =>
			 (packets_rejected := ! packets_rejected + 1;
			  ()));
		    ())
		| (0x1000, 0x4003) =>	(* MACH_RCV_TIMED_OUT. *)
		   (Buffers.recycle buffer;
		    read_timeouts := ! read_timeouts + 1)
		| (0x1000, 0x4004) =>
		   (* MACH_RCV_TOO_LARGE: message is too large for buffer *)
		   (packets_rejected := ! packets_rejected + 1;
		    local_print "mach message too large, dropping")
		| x =>
		   (packets_rejected := ! packets_rejected + 1;
		    raise_fun ("unknown return code from unsafe mach_msg " ^
			       (FoxMakestring.word32 (int_pair_to_word32 x)) ^ 
			       " in receive_timeout",
			       Local_Mach.Mach_Error 
			       (FoxMakestring.word32 (int_pair_to_word32 x)))))
	end (* let *)
  end (* local *)

(*
		14.	function set_filter
*)

  type filter = B.Filter.filter

  fun set_filter filter =
       case ! state of
	  NONE => raise_fun ("protocol not initialized", Protocol_Finalized)
	| SOME (Interface {write_port, read_port, ...}) =>
	   let fun raise_init () =
		    raise Initialization_Failed "error setting device filter"
	       val priority = if high_priority_filter then n4u100 else n4u25
	       val compiled_filter = B.Filter.make filter
	       val filter_length = ByteArray.length compiled_filter quot 2
	       val device_filter = (compiled_filter, filter_length)
	   in debug_print (fn _ => "set_filter:  [" ^
			   B.Filter.makestring filter ^ "]");
	      (Device.device_set_filter (write_port,
					 (read_port,
					  Local_Mach.MSG_TYPE_MAKE_SEND),
					 priority, device_filter))
	      handle x => handle_fun (x, "device_set_filter", raise_init)
	   end (* let *)

(*
		15.	function clear_filter
*)

  fun clear_filter () =
       set_filter B.Filter.True

(*
	16.	function initialize
*)

  fun initialize () =
       case ! state of
          NONE =>
	    (* Get a write port to the device by querying the Mach nameserver.
	       Task by pid gets a port to the task given its process id, and
	       we happen to know that it has this magic pid number. *)
           let val device_magic_number = n4uxFFFFFFFE
	       val task = task_by_pid device_magic_number
	       val wp = get_write_port (device_name, task)
	       val rp = get_read_port ()	(* Get a fresh read port. *)
	       val addr = get_ethernet_address wp
	       val count = ref 1
	       val interface =
		     Interface {address = addr,
				write_port = wp,
				read_port = rp,
				receive_handler = ref discard_packet,
				packets_sent = ref 0,
				packets_received = ref 0,
				packets_rejected = ref 0,
				read_attempts = ref 0,
				read_timeouts = ref 0,
				mach_msg_returned = ref 0,
				count = count}
	       val fork = receive_timeout (rp, count)
	   in state := SOME interface;
	      clear_filter ();		(* accept everything. *)
	      B.Scheduler.fork_limited_time fork;
	      1
	   end (* let *)
	| SOME (protocol as (Interface {count, ...})) =>
	   (* already initialized, simply bump up my reference counter. *)
	   (count := ! count + 1;
	    ! count)

(*
	17.	function finalize

*)

  fun finalize () =
       case ! state of
          SOME (Interface {count, read_port, write_port, address, ...}) =>
	   if ! count = 1 then
	    ((Device.device_close write_port
	      handle x => handle_fun (x, "device_close", fn () => ()));
	     close_port read_port;
	     state := NONE;	(* this also terminates receive_timeout *)
	     0)
	   else
	    (* We still have more than one reference to this interface,
	       so we only decrement the reference count. *)
	    (count := ! count - 1;
	     ! count)
        | NONE =>
           (local_print ("excess device closing " ^ device_name ^
			 ": no state");
	    0)

(*
	18.	functions connect, *_passive, close, abort
*)

 (* Active and passive open simply set the current handler to
    the one they have been given. *)

  fun connect ((), Handler handler) =
       case ! state of
	  (SOME (Interface {receive_handler, ...})) =>
	   let val (data_handler, _) = handler ()
	   in receive_handler := data_handler
	   end
	| _ => raise_fun ("protocol not initialized in active open",
			  Protocol_Finalized)

  fun start_passive ((), Handler handler, n) =
       case ! state of
	  (SOME (Interface {receive_handler, ...})) =>
	   (let val (data_handler, _) = handler ()
	    in receive_handler := data_handler
	    end;
	    (fn _ => (), fn _ => [()]))
	| _ => raise_fun ("protocol not initialized in start passive",
			  Protocol_Finalized)

  (* close and abort set the handler back to discard_packet *)
  fun close () =
       case ! state of 
	  (SOME (Interface {receive_handler, ...})) =>
	   receive_handler := discard_packet
	| _ => raise_fun ("protocol not initialized", Protocol_Finalized)

  val abort = close

(*
	19.	device-specific functions
*)

  fun call_state (f, name, value) =
       case ! state of
	  (SOME x) => f (x, value)
	| _ => raise_fun (name ^ ", protocol not initialized",
			  Protocol_Finalized)

  fun address_fun (Interface {address, ...}, ()) =
       B.Create.copy_create (address, 0, ByteArray.length address)

  fun sent_fun (Interface {packets_sent, ...}, ()) = ! packets_sent
  fun received_fun (Interface {packets_received, ...}, ()) = ! packets_received
  fun rejected_fun (Interface {packets_rejected, ...}, ()) = ! packets_rejected
  fun returned_fun (Interface {mach_msg_returned, ...}, ()) =
       ! mach_msg_returned
  fun attempts_fun (Interface {read_attempts, ...}, ()) = ! read_attempts
  fun timeouts_fun (Interface {read_timeouts, ...}, ()) = ! read_timeouts

  fun local_address () = call_state (address_fun, "local_address", ())
  fun packets_sent () = call_state (sent_fun, "packets_sent", ())
  fun packets_received () = call_state (received_fun, "packets_received", ())
  fun packets_rejected () = call_state (rejected_fun, "packets_rejected", ())
  fun mach_msg_returned () = call_state (returned_fun, "mach_msg_returned", ())
  fun read_attempts () = call_state (attempts_fun, "read_attempts", ())
  fun read_timeouts () = call_state (timeouts_fun, "read_timeouts", ())

(*
	20.	function send

	The implementation for send has been achieved by inlining
	device_write_request[_inband] and simplifying where possible.

	One complication is that we want to use different mechanisms
	to send short messages (we want to send them inband) and
	long messages (we want to send them out-of-band). The
	switchover is at 128 bytes.
*)

  local
   fun debug_print_send (data, length) =
        if (ByteArray.length data) < 14 then
         local_print ("error in debug_print_send, " ^
		      "bytearray shorter than format.")
        else
	 let val banner = "\nethdev.fun: making outgoing packet"
	     val format = [B.Format.String banner,
			   B.Format.String "\nTo    = ",
			   B.Format.Bytes 6,
			   B.Format.String "\nFrom  = ",
			   B.Format.Bytes 6,
			   B.Format.String "\nproto = ",
			   B.Format.Bytes 2,
			   B.Format.String "\nlength = ",
			   B.Format.String (B.V.Integer.makestring length),
			   B.Format.String "\ndata  = ",
			   B.Format.Bytes (min (45, length - 14)),
			   B.Format.String "...\n"]
	     fun byte_headers B.Format.Hex = "x"
	       | byte_headers B.Format.Binary = "b"
	       | byte_headers B.Format.Decimal = ""
	     val old_byte_headers = ! B.Format.byte_header
	     val _ = B.Format.byte_header := byte_headers
	     val list = B.Format.bytearray (format, data, 0)
	     val string = fold op^ list ""
	 in local_print string;
	    B.Format.byte_header := old_byte_headers
	 end

   (* if this were a device_write rather than a device_write_request,
      the msg_options would contain MSG_OPTION_RCV (as well as _SEND)
      and the out_size would be non-zero (typically, 40). Consult
      your nearest MIG for details. *)
   val msg_options = encode_options [Local_Mach.MSG_OPTION_SEND]
   val out_size = 0
    (* The mach_msg will be given a buffer containing three objects:
       a mode, a record number, and the data. The mode has value
       zero, which means don't wait for completion of data. The
       record number is not used (and is always zero).
       The data is the ethernet packet we are sending. *)
   val mode_type =
        Local_Mach.MSG_FIELD_HEADER {name = Local_Mach.MSG_TYPE_INTEGER_32,
				     size = 32,
				     number = 1,
				     inline = true,
				     longform = false,
				     deallocate = false}
   val rec_type =
        Local_Mach.MSG_FIELD_HEADER {name = Local_Mach.MSG_TYPE_INTEGER_32,
				     size = 32,
				     number = 1,
				     inline = true,
				     longform = false,
				     deallocate = false}
   val mode = n4u0
   val recnum = n4u0
    (* data offset is the index to which we can copy inband data.
       long_offset is the index to which we can copy the pointer
       to out-of-band-data. *)
   val data_offset = 44
   val long_offset = 52
   val long_size = long_offset + 4
   val inband_limit = 128
   val write_id = n4u2802 (* device_write *)
   val write_inband_id = n4u2803 (* device_write_inband *)

   fun print_mach_args (s, (message, _, in_size, out_size, _, _, _)) =
        (local_print (s ^ ", in_size " ^ B.V.Integer.makestring in_size ^
		      ", out_size " ^ B.V.Integer.makestring out_size ^
		      ", message: ");
	 B.V.Print.print_byte1Uarray message)

(* mach return codes are defined in
/afs/cs.cmu.edu/project/mach3/latest/release/pmax_mach/include/mach/message.h
*)

   fun send_packet (mach_args, packets_sent, assign) () =
	(assign ();
	 (case word32_to_int_pair (u_mach_msg mach_args) of
             (0, 0) =>			(* MACH_MSG_SUCCESS: Valid send *)
	      packets_sent := ! packets_sent + 1
	   | (0x1000, 0x0008) =>
       (* MACH_SEND_MSG_TOO_SMALL: data doesn't contain a complete message. *)
	      (print_mach_args ("mach-send message too small", mach_args);
	       raise_fun ("mach_send message to small in send_data",
			  Send_Failed ("message too small")))
	   | x =>
	      raise_fun ("unknown return code from unsafe mach_msg " ^
			 (FoxMakestring.word32 (int_pair_to_word32 x)) ^ 
			 " in send_data",
			 Connection_Closed ((), "Mach_msg send error"))))

   fun send_print (mach_args, packets_sent, assign, packet) () =
	(debug_print_send (B.Dyn_Array.read packet, B.Dyn_Array.size packet);
	 send_packet (mach_args, packets_sent, assign) ())

   fun build_mach_message (size, write_port) =
        let val data_total = size + data_offset
            val is_inband = data_total < inband_limit
            val (in_size, id_value, data_name, mach_message, packet, assign) =
                 if is_inband then	(* round up to full words *)
		  let val allocate = Bits.andb (~4, data_total + 3)
		      val mach_message = B.Create.create allocate
		      val data_message = B.Dyn_Array.init mach_message
                      val front = B.Dyn_Array.tail (data_message, data_offset)
		      val delta = allocate - data_total
		      val data = if delta = 0 then front
				 else B.Dyn_Array.revtail (front, delta)
		      val id = write_inband_id
		      val name = Local_Mach.MSG_TYPE_CHAR
		  in (allocate, id, name, mach_message, data, fn _ => ())
		  end
	         else
		  let val mach_message = B.Create.create long_size
                      val raw_data = B.Create.create size
                      val data = B.Dyn_Array.init raw_data
		      val id = write_id
		      val name = Local_Mach.MSG_TYPE_BYTE
         (* copy a pointer to the data into the mach message.
	    Must be done at send time, else the chances of the
	    g.c. moving the buffer are too great. *)
		      fun assign_ptr () =
		           MachIPC.assign_ptr (mach_message, long_offset,
					       raw_data);
		  in (long_size, id, name, mach_message, data, assign_ptr)
		  end
            (* max_size is a MIG convention which seems useful. *)
            val max_size = max (in_size, out_size)
            (* hdr is the mach_msg message header. *)
            val hdr =
              Local_Mach.MSG_HEADER {complex = not is_inband,
				     size = max_size,
				     local_port = Local_Mach.port_null,
				     local_right = Local_Mach.MAKE_SEND_ONCE,
				     remote_port = write_port,
				     remote_right = Local_Mach.COPY_SEND,
				     kind = Local_Mach.MSG_TYPE_NORMAL,
				     id = id_value}
         (* data name and data type are used to identify the data
            we want to send to the device driver. *)
            val data_type =
                 Local_Mach.MSG_FIELD_HEADER {name = data_name,
				              size = 8,
				              number = size,
				              inline = is_inband,
				              longform = not is_inband,
				              deallocate = false}
            val infield_headers = [mode_type, rec_type, data_type]
         (* copy the message header, mode, and recnum into the buffer *)
	in Local_Mach.set_header (hdr, mach_message);
	   Local_Mach.set_field_headers (infield_headers, mach_message);
	   FoxWord32.update (mach_message, 28, mode);
	   FoxWord32.update (mach_message, 36, recnum);
         (* prepare the tuple for mach_msg *)
	   ((mach_message, msg_options, in_size, out_size,
	     Local_Mach.port_null, 0, Local_Mach.port_null), packet, assign)
	end

  in (* local *)

  (* allocate a non-moving buffer for the data, and use it to initialize
     a new dynamic array; the two are guaranteed to share, so the
     pointer to the buffer is guaranteed not to change.  This is
     important because for large buffers we give Mach a pointer to
     the data, not the data itself. *)
   
   fun allocate_send ((), size) =
        (case ! state of
	    NONE => raise_fun ("protocol not initialized", Protocol_Finalized)
	  | SOME (Interface {write_port, packets_sent, ...}) =>
	     let val (mach_args, buffer, assign) =
	                build_mach_message (size, write_port)
	         val send_fun = if Trace.debug_on () then
		                 send_print (mach_args, packets_sent, assign,
					     buffer)
			        else
				 send_packet (mach_args, packets_sent, assign)
	     in (buffer, send_fun)
	     end)
	  handle x =>
	          (local_print ("exception " ^ System.exn_name x ^
				" in allocate_send, raising Send_Failed");
		   raise Send_Failed ("exception " ^ System.exn_name x))

  end (* local *)

 end (* struct *)

