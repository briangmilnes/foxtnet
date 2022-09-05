(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edoardo Biagioni (Edoardo.Biagioni@cs.cmu.edu)
	Brian Milnes (Brian.Milnes@cs.cmu.edu)
	Ken Cline (Kenneth.Cline@cs.cmu.edu)
        Nick Haines (Nick.Haines@cs.cmu.edu)
	Robert Findler (Robert.Findler@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

		i.	Abstract

	This will linearize the first two bytes in a Kerberos
	packet, which are always the protocol version number and
	the type of packet. (The type of packet's least significat
	bit encodes the endianness of the bytes in the packet, as well.)

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor Kerberos_Tag

		iii.	RCS Log
	
$Log: kerberos_tag.fun,v $
Revision 1.1  1994/08/25  10:24:40  robby
Initial revision

Revision 1.2  1994/08/16  13:06:31  robby
removed some #defines, left reference to C file.

Revision 1.1  94/07/14  20:29:46  robby
Initial revision

Revision 1.1  94/07/13  18:48:56  robby
Initial revision


		1.	functor Kerberos_Tag
*)
functor Kerberos_Tag(structure Ubyte1:UBYTE1_LINEARIZE)=
struct

  structure Ubyte1_Pair=Pair(structure P1=Ubyte1
			     structure P2=Ubyte1)

  datatype Auth_Msg_Type=Auth_Request | Auth_Reply | Err_Reply |
                         Appl_Request | Appl_Err

  datatype Endian=Big | Little

  datatype kerberos_tag=
    Kerberos_Tag of ubyte1 * Auth_Msg_Type * Endian
  type T=kerberos_tag

  datatype extern=Extern of ByteArray.bytearray * int
  type incoming=extern
  type outgoing=extern

  exception Does_Not_Match

  fun size _=2

  fun endian_to_ubyte1 Big=1u0
    | endian_to_ubyte1 Little=1u1

  fun ubyte1_to_endian b=
    let val b=Byte1.&&(b,1u1)
    in
      if b=1u0 then Big
      else Little
    end

(* from /usr/src/usr/misc/.kerberos/include/prot.h (no revision number) *)
  fun msg_type_to_ubyte1 Auth_Request=1u2
    | msg_type_to_ubyte1 Auth_Reply=1u4
    | msg_type_to_ubyte1 Err_Reply=1u10
    | msg_type_to_ubyte1 Appl_Request=1u6
    | msg_type_to_ubyte1 Appl_Err=1u16

  fun ubyte1_to_msg_type b=
    let val b=Byte1.&&(b,1uxfe)
    in
      case b of
	1u2 => Auth_Request
      | 1u4 => Auth_Reply
      | 1u10 => Err_Reply
      | 1u6 => Appl_Request
      | 1u16 => Appl_Err
      | _ => raise Does_Not_Match
    end

  fun marshall (Kerberos_Tag (version,msg,endian),Extern (array,p))=
    let val (Ubyte1_Pair.Extern (array,p))=
      Ubyte1_Pair.marshall ((version,Byte1.|| (msg_type_to_ubyte1 msg,
					       endian_to_ubyte1 endian)),
			    Ubyte1_Pair.Extern (array,p))
    in
      Extern (array,p)
    end

  fun unmarshall (Extern (array,p))=
    let val ((version,tag),Ubyte1_Pair.Extern (array,p))=
      Ubyte1_Pair.unmarshall (Ubyte1_Pair.Extern (array,p))
    in
      (Kerberos_Tag (version,ubyte1_to_msg_type tag,ubyte1_to_endian tag),
       Extern (array,p))
    end
end
