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
	Pittsburgh, PA 15139-3891

		i.	Abstract

	This functor just uses an old working version of George
	Necula's DES code to match the DES' signature

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	

		iii.	RCS Log
	
$Log: des_prime.fun,v $
Revision 1.2  1994/07/16  22:52:16  robby
added quad_cksum

Revision 1.1  94/07/13  19:16:39  robby
Initial revision


		1.	functor My_DES
*)

functor DES'(structure B:FOX_BASIS
	     val do_prints:bool):DES'=
struct

  structure Old_DES:OLD_DES=Old_DES(structure B=B
				    val debug=do_prints)

  type key=Old_DES.CBlock

  val string_to_key=Old_DES.string_to_key
  val ubyte1_tuple_to_key=Old_DES.ubyte1_tuple_to_cblock
  val key_to_ubyte1_tuple=Old_DES.cblock_to_ubyte1_tuple

  fun u1fvl_to_bytearray u1fvl=
    let val u1fv=Locative.locget u1fvl
      val size=Flat_Vector.flat_length u1fv
      val array=(ByteArray.array (size,0))
      fun loop 0=(Byte1.update (array,0,Flat_Vector.flat_sub(u1fv,0));
			array)
	| loop n=(Byte1.update (array,n,Flat_Vector.flat_sub(u1fv,n));
			loop (n-1))
    in loop (size-1)  end

  fun ecb_encrypt key array=
    u1fvl_to_bytearray
    (Old_DES.ecb_encrypt 
     (Locative.baview array,Old_DES.key_schedule key,true))

  fun ecb_decrypt key array=
    u1fvl_to_bytearray
    (Old_DES.ecb_encrypt 
     (Locative.baview array, Old_DES.key_schedule key,false))

  fun pcbc_encrypt key array=
    u1fvl_to_bytearray
    (Old_DES.pcbc_encrypt 
     (Locative.baview array,Old_DES.key_schedule key,key,true))
    
  fun pcbc_decrypt key array=
    u1fvl_to_bytearray
    (Old_DES.pcbc_encrypt
     (Locative.baview array, Old_DES.key_schedule key,key,false))

  fun quad_cksum key array=
    let val (checksum,_)=Old_DES.quad_cksum (Locative.baview array,key,4)
    in checksum end

end
