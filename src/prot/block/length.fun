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
	1.	functor Length

	iii.	RCS Log

$Log: length.fun,v $
Revision 1.5  1997/11/19  13:50:11  cline
109.32 compatibility

Revision 1.4  96/01/19  23:06:29  esb
adapted to the new wordarray signature.

Revision 1.3  1995/09/24  20:06:57  esb
fixed a bug whereby the status handler was being given the lower connection.

Revision 1.2  1995/06/27  19:15:20  cline
adapted to new extern.sig

Revision 1.1  1995/06/20  17:16:27  esb
Initial revision


	1.	functor Length
*)

functor Length (structure Lower: PROTOCOL
		structure Length_Extern: EXTERN
		  where type extern_in = Lower.Incoming.T
		    and type extern_out = Lower.Outgoing.T
		    and type cursor = Word.word
		 sharing type Lower.Incoming.T = Lower.Outgoing.T
		val length_word: Length_Extern.T -> Word.word
		val word_length: Word.word -> Length_Extern.T
   (* the length (in bytes) of the part of the header which precedes
      the length field.  For standard IP, the correct value is 2. *)
		val additional_length: Word.word
		val min_length: Lower.connection -> Word.word
		structure B: FOX_BASIS): PROTOCOL =
 struct
  open Lower			(* inherit all types and substructures *)

  fun word_max (a, b: Word.word) = if a > b then a else b

  local
   fun length_send (lower_send, min_send_length) packet =
	let val packet_length = Lower.Outgoing.size packet + additional_length
	    val length = word_length packet_length
	    val header_size = Length_Extern.size length;
	    val bytes = Lower.Outgoing.uninitialized header_size
	    val total_length = word_max (min_send_length,
					 packet_length + header_size)
	in Length_Extern.marshal (bytes, word_length total_length) 0w0;
	   lower_send (Lower.Outgoing.join (bytes, packet))
	end

   fun length_conn_handler upper_handler (lc as (Lower.C {send, abort,
							  extension})) =
        let val min_send_length = min_length lc
	    val connection = C {send = length_send (send, min_send_length),
				abort = abort, extension = extension}
        in upper_handler connection
        end

   fun length_data_handler upper_handler (lc as (Lower.C {send, abort,
							  extension}),
					  packet) =
        let val (length, skip) = Length_Extern.unmarshal (packet, 0w0)
	    val (_, short_packet) = Lower.Incoming.split (packet, skip)
	    val size = length_word length - additional_length - skip
	    val (result, discard) = Lower.Incoming.split (short_packet, size)
	    val min_send_length = min_length lc
	    val connection = C {send = length_send (send, min_send_length),
			      abort = abort, extension = extension}
        in upper_handler (connection, result)
        end

   fun length_status_handler upper_handler (lc as (Lower.C {send, abort,
							    extension}),
					    status) =
        let val min_send_length = min_length lc
	    val connection = C {send = length_send (send, min_send_length),
				abort = abort, extension = extension}
        in upper_handler (connection, status)
        end

   fun length_handler upper_handler key =
        let val {connection_handler, data_handler,
		 status_handler} = upper_handler key
        in {connection_handler = length_conn_handler connection_handler,
	    data_handler = length_data_handler data_handler,
	    status_handler = length_status_handler status_handler}
        end

   fun length_connect connect (address, Lower.H handler) =
        connect (address, H (length_handler handler))

   fun length_listen listen (pattern, Lower.H handler, max) =
        listen (pattern, H (length_handler handler), max)

   fun length_session session_fun (Lower.S {connect, listen, extension}) =
        session_fun (S {connect = length_connect connect,
			listen = length_listen listen,
			extension = extension})

  in (* local *)
   fun session (initialization, session_fun) =
        Lower.session (initialization, length_session session_fun)

  end (* local *)

 end (* struct *)
