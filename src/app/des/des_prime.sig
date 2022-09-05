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

	This is another view of DES's functionality.

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	signature DES'

		iii.	RCS Log
	
$Log: des_prime.sig,v $
Revision 1.2  1994/07/16  22:52:29  robby
added quad_cksum

Revision 1.1  94/07/13  19:16:45  robby
Initial revision


		1.	signature DES'
*)

signature DES'=sig
  type key

  val ecb_encrypt : key -> ByteArray.bytearray -> ByteArray.bytearray
  val ecb_decrypt : key -> ByteArray.bytearray -> ByteArray.bytearray

  val pcbc_encrypt : key -> ByteArray.bytearray -> ByteArray.bytearray
  val pcbc_decrypt : key -> ByteArray.bytearray -> ByteArray.bytearray

  val string_to_key : string -> key
  val ubyte1_tuple_to_key :
    ubyte1*ubyte1*ubyte1*ubyte1*ubyte1*ubyte1*ubyte1*ubyte1 -> key
  val key_to_ubyte1_tuple : 
    key -> ubyte1*ubyte1*ubyte1*ubyte1*ubyte1*ubyte1*ubyte1*ubyte1

  val quad_cksum : key -> ByteArray.bytearray -> ubyte4
end
