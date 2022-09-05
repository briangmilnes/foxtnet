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

	arpall.fun: Assembling the standard ARP protocol.

	ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor Arp_All
	2.	structure Hardware_Type
	3.	structure Protocol_Number
	4.	structure Hardware_Length
	5.	structure Protocol_Length
	6.	structure Arp_Opcode
	7.	structure Sender_Hardware
	8.	structure Sender_Protocol
	9.	structure Receiver_Hardware
	10.	structure Sender_Protocol

	iii.	RCS Log

$Log: arpall.fun,v $
Revision 1.4  1996/01/19  23:06:29  esb
adapted to the new wordarray signature.

Revision 1.3  1996/01/15  18:00:49  cline
removed FoxWord

Revision 1.2  1995/08/08  18:28:04  esb
adapted to new external functors.

Revision 1.1  1995/06/20  17:16:27  esb
Initial revision


	1.	functor Arp_All
*)

functor Arp_All (structure Lower: PROTOCOL
		 val hardware_type: Word16.word
		 val hardware_address_length: Word.word
		 structure Hardware_Address: EXTERN
(*
		 val local_hardware: 'a Lower.session -> Hardware_Address.T
		 val zero_hardware: Hardware_Address.T
*)
		 structure Protocol_Address: EXTERN
		 val local_protocol: unit -> Protocol_Address.T
		 structure B: FOX_BASIS): PROTOCOL =
 struct

(*
	2.	structure Hardware_Type
*)

  structure Marshal_Word8 = Protocol_Extern8 (structure In = Lower.Incoming
				      structure Out = Lower.Outgoing
				      structure B = B)

  structure Marshal_Word16 =
      Protocol_Extern16_Big (structure In = Lower.Incoming
			     structure Out = Lower.Outgoing
			     structure B = B)

  structure Hardware_Type = Constant (structure Lower = Lower
				      structure Constant = Marshal_Word16
				      val constant = hardware_type
				      val same_constant = Marshal_Word16.equal
				      structure B = B)

(*
	3.	structure Protocol_Number
*)

  structure Protocol_Number = Multiplex (structure Lower = Hardware_Type
					 structure Selector = Marshal_Word16
					 val cursor_in = fn _ => 0w0
					 val cursor_out = fn _ => 0w0
					 structure B = B
					 val name = "arp-protocol")

(*
	4.	structure Hardware_Length
*)

  val hardware_length = Word8.fromInt (Word.toInt hardware_address_length)

  structure Hardware_Length = Constant (structure Lower = Protocol_Number
					structure Constant = Marshal_Word8
					val constant = hardware_length
					val same_constant = Marshal_Word8.equal
					structure B = B)

(*
	5.	structure Protocol_Length
*)

  fun protocol_length packet =
       let val packet_size = Hardware_Length.Outgoing.size packet
	   val opcode_size = 0w2
       in Word8.fromInt (Word.toInt ((packet_size - opcode_size) div 0w2 -
				     hardware_address_length))
       end

  structure Protocol_Length = Packet_Value (structure Lower = Hardware_Length
					    structure Value = Marshal_Word8
					    val value = protocol_length
					    structure B = B)

(*
	6.	structure Arp_Opcode
*)

  structure Arp_Opcode = Send_Receive (structure Lower = Protocol_Length
				       structure Value = Marshal_Word16
				       val connect_send = Word16.fromInt 1
				       val listen_send = Word16.fromInt 2
				       val same_value = Marshal_Word16.equal
				       structure B = B)

(*
(*
	7.	structure Sender_Hardware
*)

  structure Sender_Hardware =
      Incoming_Header (structure Lower = Arp_Opcode
		       structure Value = Hardware_Address
		       val send = local_hardwre
		       structure B = B)

(*
	8.	structure Sender_Protocol
*)

  structure Sender_Protocol = Constant (structure Lower = Sender_Hardware
					structure Constant = Protocol_Address
					val constant = local_protocol ()
					val same_constant = fn _ => true
					structure B = B)

(*
	9.	structure Receiver_Hardware
*)

  structure Receiver_Hardware = Void (structure Lower = Sender_Protocol
				      val void_size = hardware_address_length
				      structure B = B)

(*
	10.	structure Sender_Protocol
*)

  structure Receiver_Protocol = Constant (structure Lower = Receiver_Hardware
					  structure Constant = Protocol_Address
					  val constant = local_protocol ()
					  val same_constant = fn _ => true
					  structure B = B)

  open Sender_Protocol
*)

  open Arp_Opcode
 end (* struct *)
