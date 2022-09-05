(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Kuo Chiang Chiang (kcchiang@cs.cmu.edu)
	Edoardo Biagioni (Edoardo.Biagioni@cs.cmu.edu)
	Brian Milnes (Brian.Milnes@cs.cmu.edu)
	Ken Cline (Kenneth.Cline@cs.cmu.edu)
        Nick Haines (Nick.Haines@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

	i.	Abstract
		
	Implements the different XDR marshal/unmarshal functions.

	ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	signature ARP_PROTOCOL_EXTERN =
	2.	functor Arp_Header

	iii.	RCS Log
	
$Log: arpheader.fun,v $
Revision 1.7  1997/11/19  13:50:11  cline
109.32 compatibility

Revision 1.6  96/02/07  19:16:18  cline
added null_hardware_address

Revision 1.5  1996/01/19  23:05:07  esb
adapted to the new wordarray signature.

Revision 1.4  1996/01/15  18:00:49  cline
removed FoxWord

Revision 1.3  1995/08/08  18:25:01  esb
separated in and out external structures.

Revision 1.2  1995/06/27  19:14:49  cline
adapted to new extern.sig

Revision 1.1  1995/06/20  16:57:52  esb
Initial revision


	1.	signature ARP_PROTOCOL_EXTERN =

	An ARP protocol extern is an extern that has an additional
	integer specifying the length of the protocol address fields.
*)

signature ARP_PROTOCOL_EXTERN =
 sig
  include EXTERN_KEY
  type regular_extern_in
  type protocol_extern_in = regular_extern_in * Word.word
  sharing type extern_in = protocol_extern_in
 end

(*
	2.	functor Arp_Header
*)

functor Arp_Header (structure In: EXTERNAL
		    structure Out: EXTERNAL
		    structure Hardware_Address: EXTERN
		      where type extern_in = In.T
		        and type extern_out = Out.T
			and type cursor = Word.word
		    structure Protocol_Address: ARP_PROTOCOL_EXTERN
		      where type regular_extern_in = In.T
		        and type extern_out = Out.T
			and type cursor = Word.word
		    val null_hardware_address: Hardware_Address.T
		    structure B: FOX_BASIS) =
 struct
  datatype T = Request of {hardware_type: Word16.word,
			   protocol_number: Word16.word,
			   sender_hardware: Hardware_Address.T,
			   sender_protocol: Protocol_Address.T,
			   receiver_protocol: Protocol_Address.T}
             | Reply of {hardware_type: Word16.word,
			 protocol_number: Word16.word,
			 sender_hardware: Hardware_Address.T,
			 sender_protocol: Protocol_Address.T,
			 receiver_hardware: Hardware_Address.T,
			 receiver_protocol: Protocol_Address.T}

  type extern_in = In.T
  type extern_out = Out.T
  type cursor = Word.word
  exception Extern

  val fixed_component = Word.fromInt (2 + 2 + 1 + 1 + 2)

  fun variable_size (hardware, protocol) =
       0w2 * (Hardware_Address.size hardware + Protocol_Address.size protocol)

  fun size (Request {sender_hardware, sender_protocol, ...}) =
       fixed_component + variable_size (sender_hardware, sender_protocol)
    | size (Reply {sender_hardware, sender_protocol, ...}) =
       fixed_component + variable_size (sender_hardware, sender_protocol)

  val request_op = Word16.fromInt 1
  val reply_op = Word16.fromInt 2

  structure Marshal_Word16 = Protocol_Extern16_Big (structure In = In
					     structure Out = Out
					     structure B = B)
  structure Marshal_Word8 = Protocol_Extern8 (structure In = In
				       structure Out = Out
				       structure B = B)

  fun single_marshal (array, hardware_type, protocol_number, opcode,
		      sender_hardware, sender_protocol,
		      receiver_hardware, receiver_protocol) cursor =
       (let val n8 = Word8.fromInt o Word.toInt
	    val hardware_length = n8 (Hardware_Address.size sender_hardware)
            val protocol_length = n8 (Protocol_Address.size sender_protocol)
        in ((Protocol_Address.marshal (array, receiver_protocol) o
	     Hardware_Address.marshal (array, receiver_hardware) o
	     Protocol_Address.marshal (array, sender_protocol) o
	     Hardware_Address.marshal (array, sender_hardware) o
	     Marshal_Word16.marshal (array, opcode) o
	     Marshal_Word8.marshal (array, protocol_length) o
	     Marshal_Word8.marshal (array, hardware_length) o
	     Marshal_Word16.marshal (array, protocol_number) o
	     Marshal_Word16.marshal (array, hardware_type)) cursor)
	 handle x =>
	         let val rpsize = Protocol_Address.size receiver_protocol
		     val rhsize = Hardware_Address.size receiver_hardware
		     val spsize = Protocol_Address.size sender_protocol
		     val shsize = Hardware_Address.size sender_hardware
		     val opsize = Marshal_Word16.size opcode
		     val plsize = Marshal_Word8.size protocol_length
		     val hlsize = Marshal_Word8.size hardware_length
		     val pnsize = Marshal_Word16.size protocol_number
		     val htsize = Marshal_Word16.size hardware_type
		     val size = rpsize + rhsize + spsize + shsize + opsize
		              + plsize + hlsize + pnsize + htsize
		     val size_string = Word.toString size ^
		                       " (" ^ Word.toString rpsize ^
		                       " + " ^ Word.toString rhsize ^
		                       " + " ^ Word.toString spsize ^
		                       " + " ^ Word.toString shsize ^
		                       " + " ^ Word.toString opsize ^
		                       " + " ^ Word.toString plsize ^
		                       " + " ^ Word.toString hlsize ^
		                       " + " ^ Word.toString pnsize ^
		                       " + " ^ Word.toString htsize ^
				       ")"
		     val exn_string = B.V.Control.exnName x
		     val cursor_string = Word.toString cursor
		     val array_string = Word.toString (Out.size array)
		 in B.V.Print.print ("arpheader.fun: exception " ^ exn_string ^
				     " while marshalling header of size " ^
				     size_string ^ " with cursor " ^
				     cursor_string ^ " and array of size " ^
				     array_string ^ "\n");
		    raise x
		 end
        end)
	 handle _ => raise Extern
         
  fun marshal (array, Request {hardware_type, protocol_number, sender_hardware,
			       sender_protocol, receiver_protocol}) =
       single_marshal (array, hardware_type, protocol_number, request_op,
		       sender_hardware, sender_protocol, null_hardware_address,
		       receiver_protocol)
    | marshal (array, Reply {hardware_type, protocol_number, sender_hardware,
			     sender_protocol, receiver_hardware,
			     receiver_protocol}) =
       single_marshal (array, hardware_type, protocol_number, reply_op,
		       sender_hardware, sender_protocol, receiver_hardware,
		       receiver_protocol)

  fun unmarshal (array, cursor) =
       (let val (hw, pr_cursor) = Marshal_Word16.unmarshal (array, cursor)
            val (prnum, hl_cursor) = Marshal_Word16.unmarshal (array,
							       pr_cursor)
            val (_, pl_cursor) = Marshal_Word8.unmarshal (array, hl_cursor)
            val (pln, op_cursor) = Marshal_Word8.unmarshal (array, pl_cursor)
            val (opcode, sh_cursor) = Marshal_Word16.unmarshal (array,
								op_cursor)
	    val proto = (array, Word.fromInt (Word8.toInt pln))
	    val (sh, sp_cursor) = Hardware_Address.unmarshal (array, sh_cursor)
	    val (sp, rh_cursor) = Protocol_Address.unmarshal (proto, sp_cursor)
	    val (rh, rp_cursor) = Hardware_Address.unmarshal (array, rh_cursor)
	    val (rp, last) = Protocol_Address.unmarshal (proto, rp_cursor)
	    val result = if opcode = request_op then
	                  Request {hardware_type = hw,
				   protocol_number = prnum,
				   sender_hardware = sh,
				   sender_protocol = sp, 
				   receiver_protocol = rp}
	                 else if opcode = reply_op then
	                  Reply {hardware_type = hw,
				 protocol_number = prnum,
				 sender_hardware = sh,
				 sender_protocol = sp, 
				 receiver_hardware = rh,
				 receiver_protocol = rp}
			 else raise Extern
        in (result, last)
        end)
	 handle _ => raise Extern

 end (* struct *)
