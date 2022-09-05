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

	length.fun: IP-compatible length protocol.

	ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor Min_Length

	iii.	RCS Log

$Log: minlength.fun,v $
Revision 1.3  1996/04/18  21:33:02  cline
fixed table of contents and comment

Revision 1.2  1996/01/19  23:06:29  esb
adapted to the new wordarray signature.

Revision 1.1  1995/06/20  17:16:27  esb
Initial revision


	1.	functor Min_Length
*)

functor Min_Length (structure Lower: PROTOCOL
		    val min_length: Lower.connection -> Word.word
		    structure B: FOX_BASIS): PROTOCOL =
 struct
  open Lower			(* inherit all types and substructures *)

  local
   fun ml_send (lower_send, min_send_length) packet =
	let val packet_length = Lower.Outgoing.size packet
	in if packet_length < min_send_length then
	    lower_send (Lower.Outgoing.join
			(packet,
			 Lower.Outgoing.uninitialized
			  (min_send_length - packet_length)))
	   else
	    lower_send packet
	end

   fun ml_data_handler upper_handler (lc as (Lower.C {send, abort, extension}),
				      packet) =
        let val min_send_length = min_length lc
	    val connection = C {send = ml_send (send, min_send_length),
				abort = abort, extension = extension}
        in upper_handler (connection, packet)
        end

   fun ml_conn_handler upper_handler
                       (lc as (Lower.C {send, abort, extension})) =
        let val min_send_length = min_length lc
	    val connection = C {send = ml_send (send, min_send_length),
				abort = abort, extension = extension}
        in upper_handler connection
        end

   fun ml_handler upper_handler key =
        let val {connection_handler, data_handler,
		 status_handler} = upper_handler key
        in {connection_handler = ml_conn_handler connection_handler,
	    data_handler = ml_data_handler data_handler,
	    status_handler = ml_data_handler status_handler}
        end

   fun ml_connect connect (address, Lower.H handler) =
        connect (address, H (ml_handler handler))

   fun ml_listen listen (pattern, Lower.H handler, max) =
        listen (pattern, H (ml_handler handler), max)

   fun ml_session session_fun (Lower.S {connect, listen, extension}) =
        session_fun (S {connect = ml_connect connect,
			listen = ml_listen listen,
			extension = extension})

  in (* local *)
   fun session (initialization, session_fun) =
        Lower.session (initialization, ml_session session_fun)

  end (* local *)

 end (* struct *)
