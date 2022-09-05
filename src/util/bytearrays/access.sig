(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Nick Haines  (Nick.Haines@cs.cmu.edu)
	Edoardo Biagioni (Edoardo.Biagioni@cs.cmu.edu)
	Brian Milnes (Brian.Milnes@cs.cmu.edu)
	Robert Findler (Robert.Findler@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

		i.	Abstract



		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	signature ACCESS

		iii.	RCS Log
	
$Log: access.sig,v $
Revision 1.7  1996/01/15  18:00:49  cline
removed FoxWord

Revision 1.6  1994/09/30  16:24:38  esb
changed to work with SML/NJ 1.05 as well as 0.93.

Revision 1.5  1994/07/01  02:55:19  danwang
Removed bytearray type.

Revision 1.4  1994/06/27  17:04:16  robby
added to the Range datatype

Revision 1.3  93/09/17  16:54:08  milnes
Simplified name of the structure.

Revision 1.2  1993/09/02  15:42:03  esb
mistakenly over-wrote some of Brian's changes, but should
be functionally equivalent.


	1.	signature ACCESS
*)

signature ACCESS =
 sig

  datatype range_spec = 
      Range of {first: int, last: int}	(*inclusive*)
(* Test includes first, but not the member of the array that passes test *)
    | Test of {first: int, test: Word8.word -> bool}
(* Length starts at first, and includes length members of the array *)
    | Length of {first: int, length: int}
    | All				(*includes all of the array*)

  exception Bad_Range

  val app_range: (range_spec * (Word8.word -> Word8.word)
		  * Word8Array.array) -> unit

  val fold_range: range_spec -> (Word8.word * 'a -> 'a) -> 
                   'a -> Word8Array.array -> 'a

  val fold_range2: range_spec -> (Word16.word * 'a -> 'a) -> 
                    'a -> Word8Array.array -> 'a

  val from_list: Word8.word list -> Word8Array.array
  val to_list: Word8Array.array -> Word8.word list
  val to_list2: Word8Array.array -> Word16.word list

  val from_string: string -> Word8Array.array
  val to_string: Word8Array.array -> string

  val list_to_range: Word8Array.array * int * Word8.word list -> unit
  val range_to_list: Word8Array.array * range_spec -> Word8.word list
  val range_to_list2: Word8Array.array * range_spec -> Word16.word list

  val string_to_range: Word8Array.array * int * string -> unit
  val range_to_string: Word8Array.array * range_spec -> string
	    
  val compare_range_to_list: Word8Array.array * range_spec * 
                              Word8.word list -> bool

  val compare_range_to_list2: Word8Array.array * range_spec *
                               Word16.word list -> bool

  (* compare_range_to_bytearray ((b1,r1),b2,offset) compares range r1 of b1 to
   * values in b2 starting at offset. *)
  val compare_range_to_bytearray: (Word8Array.array * range_spec) * 
                                   Word8Array.array * int -> bool

  val compare_ranges: (Word8Array.array * range_spec) * 
                       (Word8Array.array * range_spec) -> bool

  val fill: Word8Array.array * Word8.word -> unit
  val fill_range: Word8Array.array * range_spec * Word8.word -> unit

 end (* sig *)
