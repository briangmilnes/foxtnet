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
	1.	signature DES_TEXT
	2.      signature TEXT_EXTENDED

		iii.	RCS Log
	
$Log: text.sig,v $
Revision 1.1  1994/01/13  18:46:51  necula
Initial revision

		1.	signature DES_TEXT
*)
			      (* DES_TEXT provides a view of the DES texts. A 
                               * text means the data that will be encrypted or 
                               * decrypted  *)
signature DES_TEXT =
  sig

    type T
      
    datatype ext =
      String of string 
    | FVL of ubyte1 flat_vector locative

    val internalize : ext -> T
    val externalize : T -> ext

  end;

(*  
                2.      signature TEXT_EXTENDED
*) 
			      (* An extended view of the DES_TEXT for use 
                               * internally *)
signature TEXT_EXTENDED =
  sig

    type T
      
    datatype ext =
      String of string 
    | FVL of ubyte1 flat_vector locative

    val internalize : ext -> T
    val externalize : T -> ext
      
    structure Block : BLOCK
                              (* An empty text of given length  *)
    val empty : int -> T      
			      (* The length of a text in bytes *)
    val length : T -> int
			      (* Extracts a BLOCK from a given position in a 
                               * text. If necessary the text is considered 
                               * padded with zeroes *)
    val fetch_block : T * int -> Block.T
			      (* Puts a 64 bit block into a text at a given 
                               * position. This function implies that T is 
                               * implemented as a mutable data structure to be 
                               * allocated with empty. I did that in an 
                               * attempt to make the fuctions work reasonably 
                               * fast even for very large texts *)
    val put_block  : T * int * Block.T -> unit
			      (* High-order function used to apply repeatedly 
                               * a given (encryption) function to the entire 
                               * text viewed as a sequence of 64 bit blocks *)
    val fold : (Block.T * 'a  -> 'a) * T * 'a -> 'a
       
  end
