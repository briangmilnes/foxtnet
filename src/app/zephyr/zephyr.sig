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

	This layer implements basic zephyr send and receive operations
	with a reliability method, failure notification, as well as
	providing a method for zephyr RPCs. It uses the port
	abstractions provided in structures with the ZEPHYR_LOWER
	signature to communicate with the zephyr servers.

	ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	signature ZEPHYR


		iii.	RCS Log

$Log: zephyr.sig,v $
Revision 1.1  1994/09/03  21:14:23  danwang
Initial revision


		1.	signature ZEPHYR
*)

signature ZEPHYR =
sig

    datatype message_kind =
	UNSAFE
      | UNACKED
      | ACKED
      | SERVACK
      | SERVNACK
      | CLIENTACK
      | STAT

    datatype z_notice = Notice of
	   {kind:message_kind,
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
	    data:string list}

    type client

    val initialize: unit -> int
    val finalize: unit -> int

(*
	The functions "new_client" and "kill_client" do the obvious thing.

	The function "client_send" sends a notice and returns blocks
	until it receives the server's acknowledgement or NONE if
	there was some delivery error.

	The function "client_call" sends a request of the server and
	blocks till it receives a server's acknowledgement and a
	message containing a reply to the request. It returns the
	reply or NONE if there was some delivery error.
*)

    val new_client: (z_notice -> unit) -> client
    val client_call: client -> z_notice -> z_notice option
    val client_send: client -> z_notice -> z_notice option
    val kill_client: client -> unit

end







