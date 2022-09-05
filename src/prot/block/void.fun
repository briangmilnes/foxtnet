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

	void.fun: extend and hide a header without looking at the contents.

	ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor Void
	2.	functor Stream_Void

	iii.	RCS Log

$Log: void.fun,v $
Revision 1.3  1996/01/19  23:06:29  esb
adapted to the new wordarray signature.

Revision 1.2  1995/08/08  20:36:27  esb
commented out the stream_protocol version

Revision 1.1  1995/06/20  17:16:27  esb
Initial revision


	1.	functor Void
*)

functor Void (structure Lower: PROTOCOL
	      val void_size: Word.word
	      structure B: FOX_BASIS): PROTOCOL =
 struct

  open Lower			(* inherit all types and substructures *)

  local
   val bytes = Lower.Outgoing.uninitialized void_size

   fun void_send lower_send packet =
	lower_send (Lower.Outgoing.join (bytes, packet))

   fun void_data_handler upper_handler (C {send, abort, extension}, packet) =
        let val connection = C {send = void_send send,
			      abort = abort, extension = extension}
	    val (_, short_packet) = Lower.Incoming.split (packet, void_size)
	in upper_handler (connection, short_packet)
	end

   fun void_handler upper_handler key =
        let val {connection_handler, data_handler,
		 status_handler} = upper_handler key
        in {connection_handler = connection_handler,
	    data_handler = void_data_handler data_handler,
	    status_handler = status_handler}
        end

   fun void_connect connect (address, Lower.H handler) =
        connect (address, H (void_handler handler))

   fun void_listen listen (pattern, Lower.H handler, max) =
        listen (pattern, H (void_handler handler), max)

   fun void_session session_fun (Lower.S {connect, listen, extension}) =
        session_fun (S {connect = void_connect connect,
			listen = void_listen listen,
			extension = extension})

  in (* local *)
   fun session (initialization, session_fun) =
        Lower.session (initialization, void_session session_fun)

  end (* local *)

 end (* sig *)

(*
(*
	2.	functor Stream_Void
*)

functor Stream_Void (structure Lower: STREAM_PROTOCOL
		     val void_size: int
		     structure B: FOX_BASIS): STREAM_PROTOCOL =
 struct

  open Lower			(* inherit all types and substructures *)

  local
   val bytes = Lower.Outgoing.uninitialized void_size

   fun map_instream packet =
        let val (_, new_packet) = Lower.Incoming.split (packet, void_size)
	in new_packet
	end

   fun map_outstream packet = Lower.Outgoing.join (bytes, packet)

   fun listen_handler handler ins =
        let val (outs, result) = handler (Instream.map map_instream ins)
	in (Outstream.map map_outstream outs, result)
	end

  in
   
   fun connect (setup, address, outstream) =
        Instream.map map_instream
	             (Lower.connect (setup, address,
				     Outstream.map map_outstream outstream))

   fun listen (setup, pattern, handler) =
        Lower.listen (setup, pattern, listen_handler handler)

  end (* local *)

 end (* sig *)
*)
