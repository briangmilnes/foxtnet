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

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	

		iii.	RCS Log
	
$Log: util.sml,v $
Revision 1.3  1996/04/18  21:37:38  cline
pass seed to pcbc_encrypt and pcbc_decrypt

Revision 1.2  1996/03/22  22:41:41  necula
Completely rewritten implementation. Changed from locatives and Flat_Vector to
Word8Array and Word64 (FOXWORD).
Fixed bugs in password_to_string and pcbc_encrypt

Revision 1.1  1994/01/13  18:46:53  necula
Initial revision

		1.	
*)
			      (* Functor implementing some utilities using 
                               * core DES encryption. *)
functor DesUtilFun (structure Des : DES
		    val debug : bool) : DES_UTIL =
  struct

    structure Des = Des
    structure Block = Des.Block
		      

			 (* PASSWORD_TO_KEY produces a key from a string. The 
                          * main purpose is to produce a key from a password *)
    fun password_to_key str =
	let
	    val _ =
		if debug then
		    print ("Input password_to_key:" ^ str ^ "\n")
		else ()
			 (* The string is split into 64-bit chunks *)
	    val blist = Block.fromVector (Byte.stringToBytes str)

			 (* An initial key is produced by walking through the 
                          * text and xor-in the bits into a key. If the text 
                          * is longer than 64 bits then the direction of 
                          * walking in the key is reversed (from bit 63 to 0) 
                          * and this direction reversing goes on as we 
                          * traverse all the text. We start going to the 
                          * Right (from 0 to 63) and an empty key.  *)
	    fun fromString (str : string) =
		let
		    fun scanFunc (blk : Block.T, (rev : bool,
						  accum : Block.T)) =
			let
			    val newblk = Block.shleft(blk, 1)
			in
			    (not rev,
			     Block.xor(accum,
				       if rev then Block.reverse(blk)
				       else Block.shleft(blk, 1)))
			end
		    val (_, res) = foldl scanFunc (false, Block.zero) blist
		in
		    res
		end

			 (* The key obtained has very probably wrong parity 
                          * so fix it  *)
	    val key = Block.fix_parity(fromString str)

	    val _ = if debug then
		(print "Key :";
		 Block.print_block key) else ()
		
	    val encrypt = Des.encrypt key
	
	    fun cbc_fun (block, cksum) = encrypt(Block.xor(block, cksum))
		
	in
			 (* Compute the CBC_CKSUM of the original text using 
                          * the computed key. Fix the parity of the resulting 
                          * 64 bit cksum to make it a valid key.  *)
	    Block.fix_parity(foldl cbc_fun key blist)
	end

    

			 (* This is the PCBC_ENCRYPTION algorithm 
                          * implementation  *) 
    fun pcbc is_encrypt (key, iblock) text =
	let
	    val des = if is_encrypt then
		          Des.encrypt key
		      else
			  Des.decrypt key

	    val blist = Block.fromArray text
		
	    fun pcbc_fun (block, (acc, blist)) =
		let
		    val out = if is_encrypt then
			            des (Block.xor(block, acc))
				 else
				     Block.xor (acc, des block)
		in
		   (Block.xor(block, out), out :: blist)
		end

	    val (_, res) = foldl pcbc_fun (iblock, []) blist
	in
	  Block.toArray (rev res)  
	end
			      (* Produces the encrypt/decrypt functions *)
    val pcbc_encrypt = pcbc true
    val pcbc_decrypt = pcbc false
      
  end;




