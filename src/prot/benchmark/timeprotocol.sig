(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edoardo Biagioni (Edoardo.Biagioni@cs.cmu.edu)
	Brian Milnes (Brian.Milnes@cs.cmu.edu)
	Nick Haines  (Nick.Haines@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

		i.	Abstract

	A signature specifying a set of inputs to time a specific protocol for
   timing a general protocol.


		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	signature TIMEPROTOCOL

		iii.	RCS Log
	
$Log: timeprotocol.sig,v $
Revision 1.1  1995/06/20  17:27:21  esb
Initial revision

Revision 1.19  1995/03/10  03:49:26  esb
partial port to 1.07.

Revision 1.18  1994/08/24  22:31:06  esb
removed on_receipt, which was unused.

Revision 1.17  1994/08/17  16:32:15  esb
the benchmarks now work.

Revision 1.16  1994/06/20  20:17:33  esb
added translate_pattern.

Revision 1.15  1994/04/20  19:45:58  milnes
No change.

Revision 1.14  1994/03/29  17:44:40  milnes
Added a size operation.

Revision 1.13  1994/03/04  02:29:02  milnes
Added mtu, and many small changes.

Revision 1.12  1994/02/17  17:10:27  milnes
Checked out, reinstalled old, rechecked in to repair the afs file move problems that confused rcs.

Revision 1.10  1994/02/08  19:05:15  milnes
Changes for new whole new timing style.

Revision 1.9  1994/01/30  18:35:23  milnes
added byte, start_test, and stop_test.

Revision 1.8  1993/12/09  19:42:19  milnes
Updated to handle scheduler, proto.sig, dev.sig and timeprotocol.sig changes.

Revision 1.7  1993/12/04  20:53:35  esb
minor change to the type of on_receipt.

Revision 1.6  1993/11/16  18:06:22  esb
added a datatype to be returned by the "check" functions.

Revision 1.5  1993/11/09  22:10:49  milnes
Updated to hide the passive/active open difference in a get_connection function.

Revision 1.4  1993/10/25  17:46:47  milnes
Updated for the timing code.

Revision 1.3  1993/10/08  15:46:38  milnes
Diddled a bit and then undid the changes.

Revision 1.2  1993/09/13  22:07:51  cline
deleted '#'s from RCS log

Revision 1.1  1993/09/03  17:25:20  milnes
Initial revision

Revision 1.2  1993/08/06  15:28:00  milnes
Changed Edo to be the top dog, deservedly.

Revision 1.1  1993/06/10  21:35:35  milnes
Initial revision


		1.	signature TIME_PROTOCOL
*)

signature TIME_PROTOCOL =
 sig
  structure P: PROTOCOL
  (* Header is the lead string on all print messages. *)
  val header: string

  (* A check function will return Valid when a valid packet has been
     received, Invalid if an invalid packet has been received, and
     ignore if a valid portion of a packet has been received. *)
  datatype check_result = Valid | Invalid | Ignore

  (* Make a byte array of the data. *)
  val make_data: int -> ByteArray.bytearray 

  (* Construct the outgoing packet with that data. *)
  val make_packet: P.connection * ByteArray.bytearray -> (unit -> unit)

  (* Check that the incoming message has the given data. *)
  datatype check_data = All | Some | None
  val check_data: check_data * ByteArray.bytearray * P.incoming
                -> check_result
  (* Check anything else that must be checked. *)
  val check_other: ByteArray.bytearray * P.incoming -> check_result
  
  type byte
  (* For confirm messages, you'd like to get the first byte of the message,
     but we've built this accessor structure to hide the packet types and 
     functionality. So, here's a byte reader. *)
  val byte: P.incoming * int -> byte

  (* For debugging purposes, it is sometimes nice to call a start and
     stop function before the timing tests. *)
  val start_test: unit -> unit
  val stop_test: unit -> unit

  (* For some tests, you want to apply an operation at the connection. *)
  val on_connection: P.connection -> unit

  (* The timing code has to know how to translate IP addresses into whatever 
     format the procol uses. *)

  type ip_number
  exception Bad_Ip_Address of ip_number * string
  val translate_address: ip_number -> P.address

  val makestring_packet: P.incoming -> string

  (* The minimum amount of data that I can send. *)
  val min_packet: int

  (* The data size of the incoming message. *)

  val size: P.incoming -> ip_number 
 end
