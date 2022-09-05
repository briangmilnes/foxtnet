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

	sendreceive.fun: write a value into every outgoing packet,
	and check the value when a packet is received.  The value
	sent/received depends on whether the connection was
	established by connect or by listen.

	ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor Send_Receive

	iii.	RCS Log

$Log: sendreceive.fun,v $
Revision 1.5  1997/11/19  13:50:11  cline
109.32 compatibility

Revision 1.4  96/01/19  23:06:29  esb
adapted to the new wordarray signature.

Revision 1.3  1995/08/08  18:28:04  esb
adapted to new external functors.

Revision 1.2  1995/06/27  19:15:56  cline
adapted to new extern.sig

Revision 1.1  1995/06/20  17:16:27  esb
Initial revision


	1.	functor Send_Receive
*)

functor Send_Receive (structure Lower: PROTOCOL
		      structure Value: EXTERN
		       where type extern_in = Lower.Incoming.T
                         and type extern_out = Lower.Outgoing.T
		         and type cursor = Word.word
		      val connect_send: Value.T
		      val listen_send: Value.T
		      val same_value: Value.T * Value.T -> bool
		      structure B: FOX_BASIS): PROTOCOL =
 struct
  open Lower			(* inherit all types and substructures *)

  local

   fun sr_send (lower_send, value) packet =
        let val size = Value.size value
	    val bytes = Lower.Outgoing.uninitialized size
	in Value.marshal (bytes, value) 0w0;
	   lower_send (Lower.Outgoing.join (bytes, packet))
	end

   fun sr_data_handler (upper_handler, receive_value, send_value)
                          (C {send, abort, extension}, packet) =
        let val connection = C {send = sr_send (send, send_value),
				abort = abort, extension = extension}
	    val (current_value, skip) = Value.unmarshal (packet, 0w0)
	    val (_, short_packet) = Lower.Incoming.split (packet, skip)
	in if same_value (current_value, receive_value) then
	    upper_handler (connection, short_packet)
	   else ()
	end

   fun sr_handler (upper_handler, receive_value, send_value) key =
        let val {connection_handler, data_handler,
		 status_handler} = upper_handler key
	    val data = sr_data_handler (data_handler, receive_value,
					send_value)
        in {connection_handler = connection_handler,
	    data_handler = data, status_handler = status_handler}
        end

   fun sr_connect connect (address, Lower.H handler) =
        connect (address, H (sr_handler (handler, listen_send, connect_send)))

   fun sr_listen listen (pattern, Lower.H handler, max) =
        listen (pattern,
		H (sr_handler (handler, connect_send, listen_send)), max)

   fun sr_session session_fun (Lower.S {connect, listen, extension}) =
        session_fun (S {connect = sr_connect connect,
			listen = sr_listen listen,
			extension = extension})

  in (* local *)
   fun session (initialization, session_fun) =
        Lower.session (initialization, sr_session session_fun)

  end (* local *)

 end (* sig *)
