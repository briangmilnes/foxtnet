(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Brian Milnes (Brian.Milnes@cs.cmu.edu)
	Nick Haines  (Nick.Haines@cs.cmu.edu)
	Edoardo Biagioni <Edoardo.Biagioni@cs.cmu.edu>
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

		i.	Abstract

	This is a signature for a protocol that is a low level driver, e.g.,
 talks bytes with a device.


		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	signature RAW_DEVICE
	2.	signature DEVICE_PROTOCOL

		iii.	RCS Log
	
$Log: dev.sig,v $
Revision 1.24  1997/11/19  13:50:11  cline
109.32 compatibility

Revision 1.23  96/01/15  18:00:49  cline
removed FoxWord

Revision 1.22  1995/11/12  16:36:33  esb
made the setup type share with string.

Revision 1.21  1995/11/08  23:10:27  cline
*** empty log message ***

Revision 1.20  1995/10/02  20:59:00  esb
added a sharing constraint Setup.T = unit

Revision 1.19  1995/09/18  19:29:38  esb
minor cleanup

Revision 1.18  1995/06/20  16:48:58  esb
converted to new protocol signature.

Revision 1.17  1995/01/06  01:34:58  esb
removed set/clear_arp_filter.

Revision 1.16  1994/12/21  20:36:49  milnes
Updated for duplicate addressing.

Revision 1.15  1994/10/11  15:43:04  cline
Upgraded to work with SML/NJ 1.05b

Revision 1.14  1994/08/12  06:16:59  esb
added type allocation, recast set_arp_ip to set_arp.

Revision 1.13  1994/08/02  20:25:36  esb
adapted to new protocol signature.

Revision 1.12  1994/06/16  16:45:05  danwang
Updated to use functorized Fox_Basis

Revision 1.11  1994/06/05  18:43:27  milnes
Added control operation to set arp filter.

Revision 1.10  1994/03/07  13:11:12  esb
set control back to being unit.

Revision 1.9  94/01/30  18:15:18  milnes
added statistics.

Revision 1.8  1993/12/04  20:55:41  esb
put safe_msg and zero_timeout into the control operation.

Revision 1.7  1993/10/25  17:42:32  milnes
Added safe_msg and zero_timeout components to let us manipulate the
ethernet device.

Revision 1.6  1993/09/02  15:50:41  esb
changed types query and result to info.

Revision 1.5  1993/07/12  14:40:58  esb
changed outgoing_message from bytearray to Send_Packet.T

Revision 1.4  1993/07/10  01:16:28  esb
made incoming_message = Receive_Packet.T instead of bytearray

Revision 1.3  1993/07/07  18:46:46  milnes
Updated the printing and statistics collected in ethdev.tim.

Revision 1.2  1993/06/11  22:33:16  esb
changed Unsafe_U to U

Revision 1.1  1993/06/10  23:05:01  milnes
Initial revision


		1.	signature RAW_DEVICE
*)

signature RAW_DEVICE =
 sig
  structure External: EXTERNAL

  structure Setup: KEY where type T = string

  type session =
        {send: External.T -> unit,
         local_address: Word_Array.T,
         packets_sent: unit -> Word64.word,
         packets_received: unit -> Word64.word,
         read_timeouts: unit -> Word64.word,
         failed_sends: unit -> Word64.word,
         packets_rejected: unit -> Word64.word}

  exception Session_Already_Open

  val session: (Setup.T * (session -> ((unit -> 'a) * (External.T -> unit))))
               -> 'a

 end

(*
		2.	signature DEVICE_PROTOCOL
*)

signature DEVICE_PROTOCOL =
 sig
  include PROTOCOL

  datatype dev_session_extension =
      Dev_Session_Extension of
        {local_address: Word_Array.T,
         packets_sent: unit -> Word64.word,
         packets_received: unit -> Word64.word,
         read_timeouts: unit -> Word64.word,
         failed_sends: unit -> Word64.word,
         packets_rejected: unit -> Word64.word}

  sharing type session_extension = dev_session_extension
 end
 where type Address.T = unit
   and type Pattern.T = unit
   and type Setup.T = string