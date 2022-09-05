(*

        FoxNet: The Fox Project's Communication Protocol Implementation Effort
        Edo Biagioni (esb@cs.cmu.edu)
        Fox Project
        School of Computer Science
        Carnegie Mellon University
        Pittsburgh, Pa 15139-3891

	i.	Abstract

	iploop.fun: A multiplexer for IP that only multiplexes one
	interface but also provides a loopback interface.

	ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor Ip_Loop
	2.	sub-structures
	3.	exported types
	4.	function session


	iii.	RCS Log

$Log: iploop.fun,v $
Revision 1.18  1996/04/18  21:20:07  cline
converted hash from int to word

Revision 1.17  1996/03/12  22:23:53  esb
now accepts external setup structure.

Revision 1.16  1996/01/19  23:02:29  esb
adapted to the new wordarray signature.

Revision 1.15  1996/01/15  18:00:49  cline
removed FoxWord

Revision 1.14  1995/11/12  16:32:54  esb
adapted to new Word_Array.

Revision 1.13  1995/10/02  21:22:22  esb
adapted to new arp.sig.

Revision 1.12  1995/06/20  17:03:24  esb
major rewrite.

Revision 1.11  1995/03/10  03:46:34  esb
adapted to new vendor.sig.

Revision 1.10  1995/03/07  20:35:27  esb
updated tracing.

Revision 1.9  1995/02/04  20:40:05  robby
updated to 107

Revision 1.8  1994/12/21  20:36:49  milnes
Updated for duplicate addressing.

Revision 1.7  1994/11/22  13:58:03  milnes
Removed addressing functor arguments.

Revision 1.6  1994/11/11  18:10:29  esb
changed the functor parameters for Debug_Trace_Printer.

Revision 1.5  1994/11/10  16:06:43  milnes
Updated for the tcpipeth/addressing scheme.

Revision 1.4  1994/10/11  15:43:04  cline
Upgraded to work with SML/NJ 1.05b

Revision 1.3  1994/09/12  18:14:37  milnes
Changed a basis print to a B.V.Print.print.

Revision 1.2  1994/08/12  06:23:32  esb
added hash* , equal* , and allocation.

Revision 1.1  1994/08/03  19:17:19  esb
Initial revision

*)

(*
	1.	functor Ip_Loop
*)

functor Ip_Loop (structure Setup: KEY
		 structure Count: COUNT
		 structure External: EXTERNAL
		 structure X: PROTOCOL_EXCEPTIONS
		 val debug_level: int ref option
		 structure B: FOX_BASIS): ADDRESS_RESOLUTION_PROTOCOL =
 struct

(*
	2.	sub-structures
*)

  structure Trace = Trace (structure V = B.V
			   val debug_level = debug_level
			   val module_name = "iploop.fun"
			   val makestring = X.makestring)

  val w16to31 = Word.fromLargeWord o Word16.toLargeWord

  structure Host: KEY =
   struct
    type T = Word_Array.T
    fun equal (a, b) =
         Word_Array.W8.U_Big.F.equal (Word_Array.to8 a, Word_Array.to8 b)
    fun hash _ = 0w0
    fun makestring _ = "loop-back"
   end

  structure Protocol =
   struct
    type T = Word16.word
    val equal: (T * T -> bool) = op=
    val hash = w16to31
    val makestring = (Integer.toString o Word16.toInt)
   end

  structure Arp_Address: ARP_ADDRESS =
   struct
    type host = Host.T
    type protocol = Word16.word
    datatype address = Specific of {self: host, peer: host, protocol: protocol}
                     | Broadcast of protocol
    type T = address
    fun makestring (Specific {self, peer, protocol}) =
         "loop-back protocol number " ^ (Integer.toString o Word16.toInt) protocol
      | makestring (Broadcast protocol) =
	 "loop-back broadcast " ^ (Integer.toString o Word16.toInt) protocol
    fun equal (Specific {self = s1, peer = p1, protocol = pr1},
	       Specific {self = s2, peer = p2, protocol = pr2}) =
         Host.equal (s1, s2) andalso Host.equal (p1, p2) andalso pr1 = pr2
      | equal (Broadcast pr1, Broadcast pr2) = pr1 = pr2
      | equal _ = false
    fun hash (Specific {self, peer, protocol}) =
         Host.hash self + Host.hash peer + w16to31 protocol
      | hash (Broadcast protocol) = w16to31 protocol
   end (* struct *)
  structure Address = Arp_Address
  structure Arp_Connection_Key = Arp_Address
  structure Connection_Key = Arp_Address

  structure Arp_Pattern: ARP_PATTERN =
   struct
    type host = Host.T
    type protocol = Word16.word
    datatype pattern = Specific of {self: host, protocol: protocol}
                     | Broadcast of protocol
    type T = pattern
    fun makestring (Specific {self, protocol}) =
         "loop-back protocol number " ^ (Integer.toString o Word16.toInt) protocol
      | makestring (Broadcast protocol) =
	 "loop-back broadcast " ^ (Integer.toString o Word16.toInt) protocol
    fun equal (Specific {self = s1, protocol = p1},
	       Specific {self = s2, protocol = p2}) =
         Host.equal (s1, s2) andalso p1 = p2
      | equal (Broadcast pr1, Broadcast pr2) = pr1 = pr2
      | equal _ = false
    fun hash (Specific {self, protocol}) =
         Host.hash self + w16to31 protocol
      | hash (Broadcast protocol) = w16to31 protocol
   end (* struct *)
  structure Pattern = Arp_Pattern

  structure Count = Count 
  structure Incoming = External
  structure Outgoing = External
  structure Status = struct type T = unit fun makestring _ = "" end
  structure Setup = Setup
  structure X = X

  exception Already_Open of Connection_Key.T

(*
	3.	exported types
*)

  type arp_session_extension = {maximum_packet_size: Word.word,
				minimum_packet_size: Word.word}
  type session_extension = arp_session_extension
  type connection_extension = unit
  type listen_extension = unit

  val arp_session_extension = {maximum_packet_size = 0w1000000, (* arbitrary *)
			       minimum_packet_size = 0w0}

  datatype connection = C of {send: Outgoing.T -> unit,
			      abort: unit -> unit,
			      extension: connection_extension}

  datatype listen = L of {stop: unit -> unit, extension: listen_extension}

  datatype handler = H of Connection_Key.T
                  -> {connection_handler: connection -> unit,
	              data_handler: connection * Incoming.T -> unit,
	              status_handler: connection * Status.T -> unit}

  datatype session = S of {connect: Address.T * handler -> unit,
			   listen: Pattern.T * handler * Count.T -> listen,
			   extension: session_extension}

(*
	4.	function session

	Note that listen is really useless, since there can only be
	one connection, and that is from ourselves.  Therefore, listen
	always succeeds but never does anything.
*)

  local
   val state = ref (NONE: (connection * (connection * Incoming.T -> unit))
		          option)
   val semaphore = B.Semaphore.new ()

   fun close () = 
        (B.Semaphore.acquire semaphore;
         case ! state of
	    (SOME (c, h)) => state := NONE
	  | _ => ();
         B.Semaphore.release semaphore)

   val abort = close

   fun send packet =
        case ! state of
	   (SOME (c, h)) => h (c, packet)
	 | _ => raise X.Send "no connection"

   fun connect (address, H handler) =
        (B.Semaphore.acquire semaphore;
	 case ! state of
	    NONE =>
	     let val {connection_handler, data_handler, status_handler} =
	               ((handler address)
			handle x => 
			        (B.Semaphore.release semaphore;
				 raise x))
		 val connection = C {send = send, abort = abort,
				     extension = ()}
	     in state := SOME (connection, data_handler);
	        B.Semaphore.release semaphore;
		((connection_handler connection;
		  close ())
		 handle x => (close (); raise x))
	     end
	  | SOME x =>
	     (B.Semaphore.release semaphore;
	      raise Already_Open address))

   fun listen (pattern, handler, count) =
        L {stop = fn _ => (), extension = ()}

  in
   fun session (_, session_fun) =
        session_fun (S {connect = connect, listen = listen,
		        extension = arp_session_extension})
  end

 end (* struct *)




