(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
        George Necula (George.Necula@@cs.cmu.edu)
	Edoardo Biagioni (Edoardo.Biagioni@@cs.cmu.edu)
	Brian Milnes (Brian.Milnes@@cs.cmu.edu)
	Ken Cline (Kenneth.Cline@@cs.cmu.edu)
        Nick Haines (Nick.Haines@@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

		i.	Abstract

DES encryption support
TEXT implementation
		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor TextExtendedFun 

		iii.	RCS Log
	
$Log: text.sml,v $
Revision 1.1  1994/01/13  18:46:37  necula
Initial revision

		1.	functor TextExtendedFun
*)

			      (* Implements the text (clear and ciphered data) 
                               * for encryption/decryption as ubyte1 
                               * flat_vector locative. The text can also be 
                               * imported from a string. *)
functor TextExtendedFun (structure Block : BLOCK) : TEXT_EXTENDED  =
  struct

    type T = ubyte1 flat_vector locative


    datatype ext =
      String of string
    | FVL of T

    fun internalize (String(str)) =
      locative (Flat_Vector.flat_vector(
				 map (Byte1.from_int o ord)(explode str)))
      | internalize (FVL(x)) = x

      
    fun externalize x = FVL(x)


    structure Block = Block
			      (* Allocated empty text *)
    fun empty n = locative (Flat_Vector.flat_vector_const(1u0, n))

    fun length text = Flat_Vector.flat_length(Locative.locget(text))

			      (* Fetch a block (64 bits) from a specified 
                               * position in a text. The text is considered to 
                               * be padded with 1u0 is necessary.  *)
    fun fetch_block (text, n) =
      let
	open Locative 
	fun loop(8 , rez) = rez
	  | loop(byte, rez) =
	    let
	      val value = Byte4.from_int(Byte1.to_int(locget(locsub(text,
								    byte+n))))
		handle Subscript => 4u0
	    in
	      loop(byte+1, Block.setbits(rez, byte*8, 8, value))
	    end
 
      in
	loop(0, Block.empty)
      end

			      (* Updates 64 bits from a specified position in 
                               * a text from a block *)
    fun put_block (text, n, block) = 
      let
	open Locative 
	  fun loop 8 = ()
	    | loop byte =
	      (
	       locset(locsub(text, byte + n),
		      Byte1.from_int(Byte4.to_int(Block.extractbits(block,
								    byte*8,
								    8))));
	       loop (byte+1)
	       )
      in
	loop 0
      end

			      (* High-order fold function that work on all 
                               * blokcs contained in a text. *)
    fun fold (f, text, init) =
      let
	val len = Flat_Vector.flat_length(Locative.locget(text))
	fun loop (pos, init) =
	  if pos >= len then
	    init
	  else
	    loop(pos+8, f (fetch_block(text, pos), init))
      in
	loop (0, init)
      end
	
  end;













