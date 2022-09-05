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

	packetfilter.fun: an interface to the OSF1 packetfilter pseudo-device.



		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor Packet_Filter
	2.	types and exceptions
	3.	module Trace
	4.	c functions
	5.	internal functions raise_fun and handle_fun
	6.	fun pfopen
	7.	fun close
	8.	fun get_ethernet_address
	9.	fun set_filter
	10.	fun readi.
	11.	fun write
	12.	fun select.
	13.	fun writev

*)

(*

		iii.	RCS Log

$Log: packetfilter.fun,v $
Revision 1.17  1996/03/04  20:48:30  derby
Added writev to the packetfilter.

Revision 1.16  1996/02/28  21:12:43  cline
fixed typo (packetfilter.sig -> packetfilter.fun)

Revision 1.15  1996/02/13  15:27:26  cline
removed Word32.W (word32 wrapping hacks)

Revision 1.14  1996/02/01  19:20:59  derby
Added calls to access pty functionality: openptr forkptr and window accessors

Revision 1.13  1996/01/19  23:04:33  esb
adapted to the new wordarray signature.

Revision 1.12  1996/01/15  18:00:49  cline
removed FoxWord

Revision 1.11  1995/11/29  21:55:18  cline
ported to SML/NJ 108.13

Revision 1.10  1995/11/12  16:38:45  esb
adapted to new Word_Array.

Revision 1.9  1995/11/08  23:10:27  cline
*** empty log message ***

Revision 1.8  1995/09/18  19:29:17  esb
first running version.

Revision 1.7  1995/03/12  17:53:08  esb
adapted to new trace.sig.

Revision 1.6  1995/03/07  23:55:43  esb
removed a source of inefficiency.

Revision 1.5  1995/02/21  15:46:37  esb
eliminated debugging statements which were seriously affecting performance.

Revision 1.4  1995/02/04  20:39:46  robby
updated to 107

Revision 1.3  1995/01/14  02:32:01  esb
adapted to new packetfilter.sig

Revision 1.2  1994/11/22  13:58:38  milnes
Removed addressing functor arguments.

Revision 1.1  1994/10/20  17:57:49  cline
Initial revision


*)

(*
	1.	functor Packet_Filter
*)

functor Packet_Filter (structure B: FOX_BASIS
		       val debug_level: int ref option): PACKET_FILTER =
 struct

(*
	2.	types and exceptions
*)

  type T = Posix.FileSys.file_desc
  type filter = Word_Array.T

  exception Packet_Filter

(*
	3.	module Trace
*)

  fun makestring_exn Packet_Filter = SOME "Packet Filter"
    | makestring_exn (System.Unsafe.Assembly.SysErr (str,NONE)) =
        SOME ("SysErr (" ^ str ^ ")")
    | makestring_exn (System.Unsafe.Assembly.SysErr (str,SOME int)) =
        SOME ("SysErr (" ^ str ^ ", " ^ makestring int ^ ")")
    | makestring_exn _ = NONE

  structure Trace = Trace (structure V = B.V
			   val debug_level = debug_level
			   val module_name = "packetfilter.fun"
			   val makestring = makestring_exn)

(*
	4.	c functions
*)

  val c_function = System.Unsafe.CInterface.c_function

  val c_pfopen: string * int -> int = c_function "FoxNet" "pfopen"

  val c_pfget_address: int -> Word8Array.array = 
       c_function "FoxNet" "pfget_address"

  val c_pfset_filter: int * Word8Array.array -> unit = 
       c_function "FoxNet" "pfset_filter"

  (* the last argument to select tells us if there is a timeout,
     and, if so, what the timeout is in seconds and microseconds. *)
  val c_select: int * int list * int list * int list * (int * int) option
                -> int = c_function "FoxNet" "select"

  val c_writev : int * (Word8Array.array*int*int) list -> int = c_function "FoxNet" "writev";

  val c_openpty: int*int*int*int -> int*int = c_function "FoxNet" "openpty";

  val c_forkpty: int*int*int*int -> int*int*int = c_function "FoxNet" "forkpty";

  val c_get_winsize: int->int*int*int*int = c_function "FoxNet" "get_winsize";

  val c_set_winsize: int*int*int*int*int->unit = c_function "FoxNet" "set_winsize";

(*
	5.	internal functions raise_fun and handle_fun
*)

  fun handle_fun (x, s) =
       (Trace.print_handled (x, SOME s);
	Trace.print_raise (Packet_Filter, SOME s))

(*
	6.	fun pfopen

	The second argument to c_function "pfopen" defines the packet
	filter bits to set.  In this case,
	ENNONEXCL | ENCOPYALL = 0x30.
	For details, see /usr/include/net/pfilt.h.
*)

  fun pfopen interface =
       let val pf = ((c_pfopen (interface, 0x30))
		     handle x => handle_fun (x, "pfopen (" ^ interface ^ ")"))
       in if pf < 0 then Trace.print_raise (Packet_Filter, SOME "pfopen")
	  else Posix.FileSys.wordToFD (Word32.fromInt pf)
	  before Trace.debug_print (fn _ => "pfopen returning fd = " ^
				    makestring pf)
       end

(*
	7.	fun close
*)

  fun close pf =
       (Trace.debug_print
	(fn _ => ("closing fd " ^
		  Word32.fmt StringCvt.DEC (Posix.FileSys.fdToWord pf)));
  (* Close raises SysErr for packetfilters on OSF1 1.3/2.0 even
     though it otherwise works properly.  We ignore this exception. *)
	((Posix.IO.close pf) handle x => ()))

(*
	8.	fun get_ethernet_address
*)

  fun makestring_ethernet buffer =
       let val b0 = Word8Array.sub (buffer, 0)
	   val b1 = Word8Array.sub (buffer, 1)
	   val b2 = Word8Array.sub (buffer, 2)
	   val b3 = Word8Array.sub (buffer, 3)
	   val b4 = Word8Array.sub (buffer, 4)
	   val b5 = Word8Array.sub (buffer, 5)
	   val ts = Word8.toString
       in ts b0^":"^ts b1^":"^ts b2^":"^ts b3^":"^ts b4^":"^ts b5
       end

  fun get_ethernet_address pf =
       let val int_pf = Word32.toInt (Posix.FileSys.fdToWord pf)
	   val buffer = ((c_pfget_address int_pf)
			 handle x => handle_fun (x, "get_ethernet_address"))
	   fun create_array n =
	        if n >= Word8Array.length buffer then NONE
		else SOME (Word8Array.sub (buffer, n), n + 1)
       in Trace.debug_print (fn _ => "ethernet address (fd = " ^
			     makestring int_pf ^ ") is " ^
			     makestring_ethernet buffer);
	  Word_Array.from8 (Word_Array.W8.U_Big.F.new create_array 0)
       end

(*
	9.	fun set_filter
*)

  fun join ([], NONE) = NONE
    | join ([], SOME (head, rest)) =
       SOME (head, ([], Word_Array.W8.U_Big.F.next rest))
    | join (head :: rest, other) = SOME (head, (rest, other))

  val zero = Word8.fromInt 0
  fun set_filter (pf, filter) =
       let val filter8 = Word_Array.to8 filter
	   val length = Word8.fromInt (Word.toInt 
				       (Word_Array.W8.U_Big.F.length filter8))
	   (* filter priority - any value greater than 2 should work *)
	   val priority = Word8.fromInt 50
	   val arg = ([priority, length], Word_Array.W8.U_Big.F.next filter8)
	   val array = Word_Array.W8.U_Big.F.new join arg
	   val int_pf = Word32.toInt (Posix.FileSys.fdToWord pf)
	   val (buffer, _, _) = Word_Array.expose (Word_Array.from8 array)
       in Trace.local_print ("setting filter " ^
			     B.Format.makestring (Word_Array.from8 array) ^
			     " for fd = " ^ makestring int_pf);
	  ((c_pfset_filter (int_pf, buffer))
	   handle x => handle_fun (x, "set_filter"))
       end

(*
	10.	fun readi.
	Read from packetfilter starting at position i of buf.

  fun readi (pf,a,n,i) =
       ((System.Unsafe.SysIO.readi (pf, a, i, n))
        handle x => handle_fun (x, "readi"))

*)

  fun readi (pf, buf, count, i) =
       let val (data, first, last) = Word_Array.expose buf
	   val size = Word.toInt (last - first) + 1
       in ((Posix.IO.readArr (pf, {buf = data, i = i + Word.toInt first,
				   sz = SOME (min (count, size))}))
	   handle x => handle_fun (x, "readi"))
       end

(*
	11.	fun write
*)

  fun write (pf, buf, n) =
       let val (data, first, last) = Word_Array.expose buf
	   val size = Word.toInt (last - first) + 1
       in ((Posix.IO.writeArr (pf, {buf = data, i = Word.toInt first,
				    sz = SOME (min (n, size))}))
	   handle x => handle_fun (x, "write"));
	  ()
       end

(*
	12.	fun select.
	Unix select packet filter for reading with timeout ms milliseconds.
	Returns false if it times out, true if the file descriptor is ready.
*)

  fun select (pf, ms) =
       let val sec = ms quot 1000
	   val usec = 1000 * (ms rem 1000)
	   val int_pf = Word32.toInt (Posix.FileSys.fdToWord pf)
	   val nfds = int_pf + 1
	   val result = ((c_select (nfds, [int_pf], [], [], SOME (sec, usec)))
			 handle x => handle_fun (x, "select"))
       in result > 0
       end (* let *)
(*
	13.	fun writev
*)

  fun writev (pf, bufferList) = 
     let val pf' = Word32.toInt (Posix.FileSys.fdToWord pf)
        fun convert wordArray = 
           let val ( buffer, first, last ) = Word_Array.expose wordArray
           in
              ( buffer, Word.toInt first, Word.toInt (last - first + 0w1) )
           end
     in
        c_writev (pf', map convert bufferList)
     end
 end
