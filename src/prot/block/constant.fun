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

	constant.fun: write a constant value into every packet.

	ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor Constant

	iii.	RCS Log

$Log: constant.fun,v $
Revision 1.5  1997/11/19  13:50:11  cline
109.32 compatibility

Revision 1.4  96/01/19  23:06:29  esb
adapted to the new wordarray signature.

Revision 1.3  1995/08/08  18:28:04  esb
adapted to new external functors.

Revision 1.2  1995/06/27  19:15:08  cline
adapted to new extern.sig

Revision 1.1  1995/06/20  17:16:27  esb
Initial revision


	1.	functor Constant
*)

functor Constant (structure Lower: PROTOCOL
		  structure Constant: EXTERN
		   where type extern_in = Lower.Incoming.T
		     and type extern_out = Lower.Outgoing.T
		     and type cursor = Word.word
		  val constant: Constant.T
		  val same_constant: Constant.T * Constant.T -> bool
		  structure B: FOX_BASIS): PROTOCOL =
 struct
  open Lower			(* inherit all types and substructures *)

  local
   val bytes = Lower.Outgoing.uninitialized (Constant.size constant)

   fun constant_send lower_send packet =
        (Constant.marshal (bytes, constant) 0w0;
	 lower_send (Lower.Outgoing.join (bytes, packet)))

   fun constant_data_handler upper_handler (C {send, abort, extension},
					    packet) =
        let val connection = C {send = constant_send send,
			      abort = abort, extension = extension}
	    val (value, skip) = Constant.unmarshal (packet, 0w0)
	    val (_, short_packet) = Lower.Incoming.split (packet, skip)
	in if same_constant (value, constant) then
            upper_handler (connection, short_packet)
	   else ()			(* discard packet *)
	end

   fun constant_handler upper_handler key =
        let val {connection_handler, data_handler,
		 status_handler} = upper_handler key
        in {connection_handler = connection_handler,
	    data_handler = constant_data_handler data_handler,
	    status_handler = status_handler}
        end

   fun constant_connect connect (address, Lower.H handler) =
        connect (address, H (constant_handler handler))

   fun constant_listen listen (pattern, Lower.H handler, max) =
        listen (pattern, H (constant_handler handler), max)

   fun constant_session session_fun (Lower.S {connect, listen, extension}) =
        session_fun (S {connect = constant_connect connect,
			listen = constant_listen listen,
			extension = extension})

  in (* local *)
   fun session (initialization, session_fun) =
        Lower.session (initialization, constant_session session_fun)

  end (* local *)

 end (* sig *)
