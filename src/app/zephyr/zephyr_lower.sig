(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edoardo Biagioni (Edoardo.Biagioni@cs.cmu.edu)
	Brian Milnes (Brian.Milnes@cs.cmu.edu)
	Ken Cline (Kenneth.Cline@cs.cmu.edu)
        Nick Haines (Nick.Haines@cs.cmu.edu)
	Robby Findler (Robert.Findler@cs.cmu.edu)
	Daniel Wang (Daniel.Wang@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

	i.	Abstract

	This layer is responsible for establishing and maintaining the
	connection to the zephyr servers. It also attempts to implement
	some of the basic functionality of zhm and libzephyr.a. It
	provides port abstractions which act as a reliable way to
	communicate with the zephyr server. It also acknowledges messages
	received from the server by generating CLIENTACKs when appropriate.

	ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	signature ZEPHYR_LOWER

	iii.	RCS Log

$Log: zephyr_lower.sig,v $
Revision 1.1  1994/09/03  21:12:55  danwang
Initial revision


		1.	signature ZEPHYR_LOWER
*)

signature ZEPHYR_LOWER =
sig

    datatype z_packet = Packet of
               {kind:ubyte4,
                auth:ubyte4,
	 authent_len:ubyte4,
       authenticator:ByteArray.bytearray,
	       class:string,
	    instance:string,
	      opcode:string,
	      sender:string,
	    recipient:string,
      default_format:string,
	    checksum:ubyte4,
	   notice_id:{ip_address:ubyte4,time_sec:ubyte4,time_usec:ubyte4},
		data:string list}
    type port

    val initialize: unit -> int
    val finalize: unit -> int

(*
	"new_port" allocates a port "port_open", "port_send", and
	"port_close" do the obvious and return a boolean value on success or
	failure of the operation.

	N.B. Opening a port more than once should fail.
*)
    val new_port: unit -> port
    val port_open: port -> (z_packet -> unit) -> bool
    val port_send: port -> z_packet -> bool
    val port_close: port -> bool

end
