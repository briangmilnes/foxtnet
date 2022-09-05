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

		Definition of IP options.

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	signature IP_OPTION

		iii.	RCS Log
	
$Log: ipoption.sig,v $
Revision 1.7  1996/01/19  23:02:29  esb
adapted to the new wordarray signature.

Revision 1.6  1996/01/15  18:00:49  cline
removed FoxWord

Revision 1.5  1995/09/26  16:10:59  esb
minor change in types.

Revision 1.4  1995/09/26  15:49:43  esb
added traceroute option.

Revision 1.3  1995/08/08  18:22:47  esb
changed option to ip_option.

Revision 1.2  1995/06/23  19:55:30  esb
first version of IP that compiles with the new PROTOCOL signature.

Revision 1.1  1995/06/20  17:02:38  esb
Initial revision


		1.	signature IP_OPTION
*)

signature IP_OPTION = 
 sig 

  type ip_number = Word32.word

  type address_time = {ip: ip_number, time: Word32.word}

  datatype 'a updatable_array = UA of {previous: 'a list, available: 'a list}

  datatype time_stamp = Stamp of Word32.word updatable_array
                      | Record_Stamp of address_time updatable_array
		      | Given_Stamp of address_time updatable_array

  datatype ip_option =
     Loose_Route of ip_number updatable_array
   | Strict_Route of ip_number updatable_array
   | Record_Route of ip_number updatable_array
   | Time_Stamp of {stamps: time_stamp, overflow: int}
   | Traceroute of {id: Word16.word, ip: ip_number,
		    outbound: Word16.word, return: Word16.word}
   | Other_Option of {option_type: Word8.word, send_in_fragments: bool,
		      contents: Word_Array.T}

  val send_in_fragments: ip_option -> bool

  val makestring_ip_number: ip_number -> string
  val makestring: ip_option -> string
  val makestrings: ip_option list -> string

  val equal: ip_option list * ip_option list -> bool

  val option_position: ip_option list * Word.word -> ip_option option

 end (* sig *)
