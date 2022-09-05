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
	1.	signature DES

		iii.	RCS Log
	
$Log: old_des.sig,v $
Revision 1.2  1994/07/13  19:17:00  robby
added ubyte1_tuple_to_key function to use with kerberos

Revision 1.1  94/06/17  13:15:33  robby
Initial revision

Revision 1.2  93/12/28  14:00:39  necula
Changed the interface to a functional one.

Revision 1.1  93/12/22  16:43:26  necula
Initial revision


		1.	signature DES
*)


			      (* DES signature *)
signature OLD_DES = sig

                              (* Basic type for a DES key *)
  type CBlock
                              (* Basic type for a DES key schedule *)
  type Schedule


    exception Key_Parity      (* Raised by key_schedule if the key is not 
                               * valid. Shouldn't happen if the key was 
                               * generated with string_to_key *)

    
			      (* Function to build a key from string *)
    val string_to_key : string -> CBlock (*ubyte4*ubyte4*)

			      (* Function to build a CBlock. The flat_vector 
                               * must be 8 bytes long  *)
    val ubyte_to_cblock : ubyte1 flat_vector locative -> CBlock

			      (* Function to unbuild a CBlock. The output 
                               * flat_vector will be 8 bytes long  *)
    val cblock_to_ubyte : CBlock -> ubyte1 flat_vector locative 
      
			      (* Function to build the schedule. The input 
                               * must be a valid key otherwise Key_Parity 
                               * might be raised *)
    val key_schedule : CBlock -> Schedule

			      (* Function to do the ecb encryption. The 
                               * parameters have the following meaning: *)
			      (*  input - 8 bytes long input block *)
			      (*  schedule - build with key_schedule from a 
                               *             valid key *)
			      (*  is_encrypt - a boolean specifying encryption 
                               *            if true or decryption otherwise *)
			      (*  output - 8 bytes of output *)
    val ecb_encrypt : ubyte1 flat_vector locative * Schedule *  bool ->
                      ubyte1 flat_vector locative 

			      (* Function to do the pcbc encryption. The 
                               * parameters have the following meaning: *)
			      (*  input - input block *)
			      (*  schedule - build with key_schedule from a 
                               *             valid key *)
			      (*  seed  - a secret seed  *)
			      (*  is_encrypt - a boolean specifying encryption 
                               *            if true or decryption otherwise *)
			      (* output - The output. The input string is 
                               * padded with 1u0 to a multiple of 8 and for 
                               * each 8 bytes in the padded input string 8 
                               * bytes will be generated.  *)
		      
    val pcbc_encrypt : ubyte1 flat_vector locative * Schedule * CBlock *
                       bool -> ubyte1 flat_vector locative 
      
			      (* Function to compute the quad_cksum. The 
                               * parameters have the following meaning: *)
			      (*  input - input block *)
			      (*  seed  - a secret seed  *)
			      (* olen - how many bytes of output should be 
                               * generated in output string. Must be between 
                               * 4-32  *)
		       
			      (*  The function will return a pair consisting 
                               * from the first 4 bytes of the checksum packed 
                               * as a ubyte4 and a flat_vector containing all 
                               * requested bytes *)
      
    val quad_cksum : ubyte1 flat_vector locative * CBlock * int ->
                     ubyte4 * ubyte1 flat_vector locative 


    val ubyte1_tuple_to_cblock : 
      ubyte1*ubyte1*ubyte1*ubyte1*ubyte1*ubyte1*ubyte1*ubyte1 -> CBlock
    val cblock_to_ubyte1_tuple : 
      CBlock -> ubyte1*ubyte1*ubyte1*ubyte1*ubyte1*ubyte1*ubyte1*ubyte1
end
