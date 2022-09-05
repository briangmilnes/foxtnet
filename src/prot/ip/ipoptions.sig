(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edoardo Biagioni (Edoardo.Biagioni@cs.cmu.edu)
	Brian Milnes (Brian.Milnes@cs.cmu.edu)
	Ken Cline (Kenneth.Cline@cs.cmu.edu)
        Nick Haines (Nick.Haines@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

		i.	Abstract

	


		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	signature IP_OPTION
	2.	signature IP_OPTION_INTERNAL

		iii.	RCS Log
	
$Log: ipoptions.sig,v $
Revision 1.7  1995/03/24  01:42:59  esb
changed the parameters to the parse function.

Revision 1.6  1994/10/11  15:43:04  cline
Upgraded to work with SML/NJ 1.05b

Revision 1.5  1994/08/02  20:32:38  esb
adapted to new protocol signature.

Revision 1.4  1994/06/23  14:33:24  danwang
Changed Byte#.ubytes to ubyte#.

Revision 1.3  1994/05/23  14:19:42  milnes
Installed Edo's naming changes in the old revision, plus changed
options to take the byte array of the message header, instead of
the packet.

Revision 1.1  1994/05/03  21:09:19  esb
Initial revision

		1.	signature IP_OPTION
*)

signature IP_OPTION = 
 sig 

  type ip_number

   (* The IP protocol allows clients to specify options with the 
      set_option control operation, and to receive options with the
      option_handler control operation. The options are lists of the 
      T data structure below. The input side may raise an 
      exception if an input option is not meaningful. *)

  datatype time_stamp =
      Stamp of FoxWord32.word list
    | Record_Stamp of {ip: ip_number, time: FoxWord32.word} list
    | Given_Stamp of {ip: ip_number, time: FoxWord32.word option} list

  datatype T = 
     Loose_Route of {length: FoxWord8.word, position: FoxWord8.word,
		     route: ip_number list}
   | Strict_Route of {length: FoxWord8.word, position: FoxWord8.word,
		      route: ip_number list}
   | Record_Route of {length: FoxWord8.word, position: FoxWord8.word,
		      route: ip_number list}
   | Time_Stamp of {length: FoxWord8.word, position: FoxWord8.word,
		    overflow: FoxWord8.word, stamps: time_stamp}

  (* Two functions for printing options, as they often occur in lists. *)
  val makestring_option: T -> string
  val makestring_options: T list -> string

  (* A function for parsing options. *)

  exception Illegal_Option of string

 end (* sig *)

(*

	2.	signature IP_OPTION_INTERNAL
*)

signature IP_OPTION_INTERNAL =
 sig
  include IP_OPTION

  val parse: {data: ByteArray.bytearray, start: int, length: int} -> T list
  val receive_time_stamp: T list * ip_number -> T list
  val emit: T list -> ByteArray.bytearray * int
  val check: T list -> bool

 end (* sig *)
