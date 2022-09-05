(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edoardo Biagioni (esb@cs.cmu.edu)
	Brian Milnes (Brian.Milnes@cs.cmu.edu)
	Nick Haines  (Nick.Haines@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

		i.	Abstract

	This file implments a simulated network wire. 


		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor Wire

		iii.	RCS Log
	
$Log: wire.fun,v $
Revision 1.37  1997/06/04  11:36:55  esb
disabled an optimization (handlers := new_table) that causes a storage
leak under the current SML/NJ.

Revision 1.36  97/04/18  16:07:47  esb
replaced "suspend (resume)" with "yield".

Revision 1.35  97/03/27  13:46:03  esb
adapted to new coro.sig.

Revision 1.34  96/04/18  21:27:12  cline
converted hash from int to word

Revision 1.33  1996/03/11  20:06:57  cline
updated for 109.6

Revision 1.32  1996/01/19  23:04:53  esb
adapted to the new wordarray signature.

Revision 1.31  1996/01/15  18:44:53  cline
updated for sml 108.19

Revision 1.30  1995/11/12  16:36:57  esb
adapted to new Word_Array.

Revision 1.29  1995/09/13  15:32:02  esb
changed to print every packet in trace mode

Revision 1.28  1995/06/29  18:20:22  esb
adapted to new wordarray.

Revision 1.27  1995/06/20  16:48:58  esb
converted to new protocol signature.

Revision 1.26  1995/03/12  17:53:50  esb
adapted to new trace.sig.

Revision 1.25  1995/03/07  20:37:42  esb
updated tracing.

Revision 1.24  1995/02/04  20:39:39  robby
updated to 107

Revision 1.23  1995/01/18  21:03:38  esb
adapted to new COROUTINE signature.

Revision 1.22  1994/11/22  13:58:03  milnes
Removed addressing functor arguments.

Revision 1.21  1994/10/11  15:43:04  cline
Upgraded to work with SML/NJ 1.05b

Revision 1.20  1994/08/24  22:25:08  esb
added print statements.

Revision 1.19  1994/08/02  20:25:36  esb
adapted to new protocol signature.

Revision 1.18  1994/07/01  02:34:49  danwang
Moved control structures into Fox_Basis.

Revision 1.17  1994/05/10  07:45:48  esb
adapted to new store.sig.

Revision 1.16  94/04/26  17:58:18  esb
adapted to new COROUTINE signature.

Revision 1.15  94/03/16  19:57:57  esb
improvements in error messages.

Revision 1.14  94/03/02  21:18:34  esb
added some debugging statements.

Revision 1.13  1994/01/17  18:12:18  esb
interface changes.

Revision 1.12  1994/01/13  16:22:56  milnes
Added kills for low priority threads.

Revision 1.11  93/12/04  20:56:47  esb
adapted to use low-priority coroutines instead of polling functions.

Revision 1.10  1993/10/25  19:36:07  cline
removed .U from Byte[421].U

Revision 1.9  1993/10/09  00:10:07  esb
removed a tracing message that made it very hard to trace large programs.

Revision 1.8  1993/10/06  02:51:08  esb
adapted to new store module.

Revision 1.7  1993/09/17  16:44:12  milnes
Removed #s and changed default parameters.

Revision 1.6  1993/09/02  15:53:59  esb
added the Fox Basis B.

Revision 1.5  1993/06/21  01:43:38  esb
changed to reflect the new store.sig and store.fun

Revision 1.4  1993/06/18  15:49:33  esb
fixed an infinite-loop bug in busy_wait

Revision 1.3  1993/06/15  23:03:14  esb
forgot one little do_if_debug

Revision 1.2  1993/06/15  23:00:07  esb
added do_if_debug, and a handler for exceptions generated in the
packet handler

Revision 1.1  1993/06/10  23:05:01  milnes
Initial revision


		1.	functor Wire
*)

functor Wire (structure B: FOX_BASIS
	      val debug_level: int ref option) : WIRE =
 struct
  structure Trace = Trace (structure V = B.V
			   val debug_level = debug_level
			   val module_name = "wire.fun"
			   val makestring = fn _ => NONE)

  type key = Word48.word

  type value = Word_Array.T -> unit

  val mask = Word48.fromInt 0xffffff
  val byte_mask = Word48.fromInt 0xff
  fun hash addr = Word.fromLargeWord (Word48.toLargeWord
				      (Word48.andb (addr, mask)))

  fun eq_key (k1, k2: key) = k1 = k2

  val handlers = ref ((B.Store.new (hash, eq_key)): (key, value) B.Store.T)

  val current = ref (NONE: Word_Array.T option)

  fun packet_to_key packet =
       let val p48 = Word48_Array.to packet
       in Word48_Array.U_Big.F.head p48
       end

  fun makestring_key key =
       let val makes = Int.toString o Word48.toInt 
	   fun mask n = Word48.andb (n, byte_mask)
	   val a0 = makes (mask (Word48.>> (key, 0w40)))
	   val a1 = makes (mask (Word48.>> (key, 0w32)))
	   val a2 = makes (mask (Word48.>> (key, 0w24)))
	   val a3 = makes (mask (Word48.>> (key, 0w16)))
	   val a4 = makes (mask (Word48.>> (key,  0w8)))
	   val a5 = makes (mask (Word48.>> (key,  0w0)))
       in a0 ^ ":" ^ a1 ^ ":" ^ a2 ^ ":" ^ a3 ^ ":" ^ a4 ^ ":" ^ a5
       end

  fun makestring_packet (_, 0) = ""
    | makestring_packet (NONE, count) = ""
    | makestring_packet (SOME (head, rest), count) =
       Word8.toString head ^ "." ^
       makestring_packet (Word_Array.W8.U_Big.F.next rest, count - 1)

  fun string_key (k, v) = makestring_key k

  val bcast_key = Word48.- (Word48.fromInt 0, Word48.fromInt 1)


  fun deliver (key, handler, packet) =
       (Trace.debug_print (fn _ => "delivering packet to handler for " ^
			   makestring_key key);
	((handler packet)
	 handle x =>
	         if B.V.Control.exnName x = "Receive" then ()
		 else Trace.print_handled (x, SOME "handler")))

  fun deliver_map packet (key, handler) =
       (deliver (key, handler, packet);
	handler)
  
  fun busy_wait () =
       case ! current of
          NONE => 
	   (B.Scheduler.fork busy_wait;
	    ())
        | SOME p =>
           (current := NONE;
	    B.Scheduler.fork busy_wait;
	    Trace.trace_print (fn _ => "busy_wait (), dispatching packet " ^
			       makestring_packet
			       (Word_Array.W8.U_Big.F.next (Word_Array.to8 p),
				100));
	    let val key = packet_to_key p
	    in if key = bcast_key then
	        (Trace.debug_constant_string "broadcast, calling all handlers";
		 B.Store.map (deliver_map p) (! handlers);
		 ())
	       else
		case B.Store.look (! handlers, key) of
		   NONE =>
		    Trace.debug_print (fn _ =>
				       "no handler for packet with key " ^
				       makestring_key key ^
				       "\nhandlers are " ^
				       B.Store.makestring (! handlers,
							   string_key, ", "))
		 | SOME (new_table, f) =>
		    ( (* handlers := new_table;  -- storage leak *)
		     deliver (key, f, p));
	       Trace.debug_print (fn _ => "busy_wait done")
	    end)

  fun register (p, f) =
       let val key = packet_to_key p
           val h = ! handlers
       in B.Scheduler.fork busy_wait;
          Trace.debug_print (fn _ => "registering key " ^ makestring_key key);
          case B.Store.look (h, key) of
             NONE => (handlers := B.Store.add (h, key, f); true)
           | SOME _ =>
	      (Trace.local_print ("key " ^ makestring_key key ^
				  " already registered");
	       false)
       end

  fun unregister p =
       let val key = packet_to_key p
           val h = ! handlers
       in Trace.debug_print (fn _ => "un-registering key " ^
			     makestring_key key);
          case B.Store.look (h, key) of
             SOME _ => (handlers := B.Store.remove (h, key); true)
           | NONE => false
       end

  fun send p =
       let val _ = Trace.debug_print
	               (fn _ => "in send, packet size " ^
			Word.toString
			 (Word_Array.W8.U_Big.F.length (Word_Array.to8 p)))
	   val key = packet_to_key p
           val h = ! handlers
       in case ! current of
	     NONE => ()
	   | SOME _ => 
	      Trace.trace_print (fn _ => "overwriting packet on the wire");
          current := SOME p;
   (* let other threads, particularly the receiver thread, be scheduled. *)
          Trace.debug_constant_string "yielding";
          B.Scheduler.yield ()
       end

 end (* struct *)
