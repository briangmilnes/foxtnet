(*

        FoxNet: The Fox Project's Communication Protocol Implementation Effort
        Edo Biagioni (esb@cs.cmu.edu)
	Ken Cline (Ken.Cline@cs.cmu.edu)
        Nick Haines (nickh@cs.cmu.edu)
        Brian Milnes (milnes@cs.cmu.edu)
        Fox Project
        School of Computer Science
        Carnegie Mellon University
        Pittsburgh, Pa 15139-3891

	i.	Abstract

	multiplex.fun

	ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor Multiplex

	iii.	RCS Log

$Log: multiplex.fun,v $
Revision 1.7  1995/10/04  18:52:49  esb
added the "undelivered" call required by the new conn.fun.

Revision 1.6  1995/09/13  15:33:35  esb
adapted to new conn.fun

Revision 1.5  1995/08/08  18:28:04  esb
adapted to new external functors.

Revision 1.4  1995/07/21  12:52:10  esb
adapted to new conn.fun.

Revision 1.3  1995/06/27  19:15:38  cline
adapted to new extern.sig

Revision 1.2  1995/06/27  16:57:00  esb
adapted to new conn.fun.

Revision 1.1  1995/06/20  17:16:27  esb
Initial revision


	1.	functor Multiplex
*)

functor Multiplex (structure Lower: PROTOCOL
		   structure Selector: EXTERN_KEY
		    sharing type Lower.Incoming.T = Selector.extern_in 
			and type Lower.Outgoing.T = Selector.extern_out 
		   val cursor_in: Selector.extern_in -> Selector.cursor
		   val cursor_out: Selector.extern_out -> Selector.cursor
		   structure B: FOX_BASIS
		   val name: string): PROTOCOL =
 struct

  local
   structure Address: KEY =
    struct
     type T = Selector.T * Lower.Address.T
     fun makestring (selector, lower) = 
          Selector.makestring selector ^ "," ^ Lower.Address.makestring lower
     fun equal ((selector1, lower1), (selector2, lower2)) =
          Selector.equal (selector1, selector2) andalso
          Lower.Address.equal (lower1, lower2)
     fun hash (selector, lower) =
          Selector.hash selector + Lower.Address.hash lower
    end

   structure Pattern: KEY =
    struct
     type T = Selector.T option * Lower.Pattern.T
     fun makestring (SOME selector, lower) = 
          Selector.makestring selector ^ ":" ^ Lower.Pattern.makestring lower
       | makestring (NONE, lower) = Lower.Pattern.makestring lower
     fun equal ((NONE, lower1), (NONE, lower2)) =
          Lower.Pattern.equal (lower1, lower2)
       | equal ((SOME selector1, lower1), (SOME selector2, lower2)) =
          Selector.equal (selector1, selector2) andalso
          Lower.Pattern.equal (lower1, lower2)
       | equal _ = false
     fun hash (_, lower) = Lower.Pattern.hash lower
    end

   structure Connection_Key: KEY =
    struct
     type T = Selector.T * Lower.Connection_Key.T
     fun makestring (selector, lower) = 
          Selector.makestring selector ^ "," ^
	  Lower.Connection_Key.makestring lower
     fun equal ((selector1, lower1), (selector2, lower2)) =
          Selector.equal (selector1, selector2) andalso
          Lower.Connection_Key.equal (lower1, lower2)
     fun hash (selector, lower) =
          Selector.hash selector + Lower.Connection_Key.hash lower
    end

   structure Status: PRINTABLE =
    struct
     type T = unit
     fun makestring () = "no status"
    end

   type connection_extension = unit
   type listen_extension = unit
   type session_extension = unit

   val debug_level = NONE
   structure Trace = Trace (structure V = B.V
			    val debug_level = debug_level
			    val module_name = "multiplex.fun (" ^ name ^ ")"
			    val makestring = Lower.X.makestring)

   fun lower_setup setup = setup
   fun init_proto _ = ((), ())
   fun fin_proto _ = ()
   fun resolve (_, (selector, lower)) = SOME lower
   fun make_key (_, (selector, lower), lower_key, _) = (selector, lower_key)
   fun map_pattern (_, (_, lower), _) = SOME ((), lower)
   fun match (_, (SOME selector1, lower_pat), _, (selector2, lower_key)) =
        Selector.equal (selector1, selector2)
     | match (_, (NONE, _), _, _) = true
   fun init_connection _ = ((), ())
   fun fin_connection _ = ()

   fun send ((selector, lower_key), ()) packet =
	(let val bytes = Lower.Outgoing.uninitialized (Selector.size selector)
	 in Selector.marshal (bytes, selector) (cursor_out bytes);
	    [Lower.Outgoing.join (bytes, packet)]
	 end)
	  handle x => Trace.print_raise_again (x, SOME "send")

   fun identify (lower_key, _) packet =
	(let val (selector, size) =
	           Selector.unmarshal (packet, cursor_in packet)
	 in [(selector, lower_key)]
	 end)
	  handle x => Trace.print_raise_again (x, SOME "identify")

   fun receive ((selector, lower_key), ()) =
        (let val size = Selector.size selector
	     fun process packet =
	          let val (_, result) = Lower.Incoming.split (packet, size)
		  in SOME result
		  end
	 in process
	 end)
       handle x => Trace.print_raise_again (x, SOME "receive")

   fun undelivered _ _ = ()

   fun lower_status (_, lower_key) status =
        Trace.local_print ("got lower status " ^
			   Lower.Status.makestring status ^
			   " on " ^ Lower.Connection_Key.makestring lower_key)

   structure Conn = Connection (structure Lower = Lower
				structure Setup = Lower.Setup
				structure Address = Address
				structure Pattern = Pattern
				structure Connection_Key = Connection_Key
				structure Incoming = Lower.Incoming
				structure Outgoing = Lower.Outgoing
				structure Status = Status
				structure Count = Lower.Count
				structure X = Lower.X
				type connection_extension = unit
				type listen_extension = unit
				type session_extension = unit
				type connection_state = unit
				type protocol_state = unit
				val lower_setup = lower_setup
				val init_proto = init_proto
				val fin_proto = fin_proto
				val resolve = resolve
				val make_key = make_key
				val map_pattern = map_pattern
				val match = match
				val init_connection = init_connection
				val fin_connection = fin_connection
				val send = send
				val identify = identify
				val receive = receive
				val undelivered = undelivered
				val lower_status = lower_status
				structure B = B
				val module_name = "multiplex.fun"
				val debug_level = debug_level)

  in
   open Conn
  end (* local *)

 end (* struct *)
