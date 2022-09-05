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

	packetvalue.fun: write a value into every packet.  Do NOT
	check the value when a packet is received.

	ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor Packet_Value

	iii.	RCS Log

$Log: packetvalue.fun,v $
Revision 1.5  1997/11/19  13:50:11  cline
109.32 compatibility

Revision 1.4  96/01/19  23:06:29  esb
adapted to the new wordarray signature.

Revision 1.3  1995/08/08  18:28:04  esb
adapted to new external functors.

Revision 1.2  1995/06/27  19:15:47  cline
adapted to new extern.sig

Revision 1.1  1995/06/20  17:16:27  esb
Initial revision


	1.	functor Packet_Value
*)

functor Packet_Value (structure Lower: PROTOCOL
		      structure Value: EXTERN
		       where type extern_in = Lower.Incoming.T
		         and type extern_out = Lower.Outgoing.T
		         and type cursor = Word.word
		      val value: Lower.Outgoing.T -> Value.T
		      structure B: FOX_BASIS): PROTOCOL =
 struct
  open Lower			(* inherit all types and substructures *)

  local

   fun value_send lower_send packet =
        let val current_value = value packet
	    val size = Value.size current_value
	    val bytes = Lower.Outgoing.uninitialized size
	in Value.marshal (bytes, current_value) 0w0;
	   lower_send (Lower.Outgoing.join (bytes, packet))
	end

   fun value_data_handler upper_handler (C {send, abort, extension},
					    packet) =
        let val connection = C {send = value_send send,
			      abort = abort, extension = extension}
	    val (current_value, skip) = Value.unmarshal (packet, 0w0)
	    val (_, short_packet) = Lower.Incoming.split (packet, skip)
	in upper_handler (connection, short_packet)
	end

   fun value_handler upper_handler key =
        let val {connection_handler, data_handler,
		 status_handler} = upper_handler key
        in {connection_handler = connection_handler,
	    data_handler = value_data_handler data_handler,
	    status_handler = status_handler}
        end

   fun value_connect connect (address, Lower.H handler) =
        connect (address, H (value_handler handler))

   fun value_listen listen (pattern, Lower.H handler, max) =
        listen (pattern, H (value_handler handler), max)

   fun value_session session_fun (Lower.S {connect, listen, extension}) =
        session_fun (S {connect = value_connect connect,
			listen = value_listen listen,
			extension = extension})

  in (* local *)
   fun session (initialization, session_fun) =
        Lower.session (initialization, value_session session_fun)

  end (* local *)

 end (* sig *)
