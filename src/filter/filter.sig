(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Brian Milnes (Brian.Milnes@cs.cmu.edu)
	Nick Haines  (Nick.Haines@cs.cmu.edu)
	Edoardo Biagioni <Edoardo.Biagioni@cs.cmu.edu>
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

		i.	Abstract

	An SML equivalent to the stanford packet filter, which was
	actually designed here at CMU.


		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	signature FILTER

		iii.	RCS Log
	
$Log: filter.sig,v $
Revision 1.10  1996/01/15  18:00:49  cline
removed FoxWord

Revision 1.9  1995/09/18  19:26:01  esb
now compiles to Word_Array.T instead of ByteArray.bytearray.

Revision 1.8  1995/03/07  20:22:51  esb
eliminated the type device_filter.

Revision 1.7  1995/01/14  02:24:20  esb
fixed the filter, not tested yet.

Revision 1.6  1994/12/21  20:36:49  milnes
Updated for duplicate addressing.

Revision 1.5  1994/09/30  16:12:12  esb
use FoxWord16 instead of Byte2

Revision 1.4  1994/06/05  18:40:06  milnes
Added some missing filter operations that make writing filters easier.

Revision 1.3  1993/10/25  19:29:11  cline
removed .U from Byte[421].U

Revision 1.2  1993/08/23  22:09:03  esb
added the makestring function.

Revision 1.1  1993/06/10  22:29:09  milnes
Initial revision


		1.	signature FILTER

	The packet filter is a stack machine language that is allowed
	to introduce literals, extract fields from packets in the
	message, and perform simple operations on them. This interface
	allows for the creation of filters as a recursive data type
	for use inside of SML, e.g., filter_packet. It also allows the
	construction of a system_filter type from a filter, which can
	then be passed to the Mach kernel, allowing it to perform the
	filtering.
*)

signature FILTER =
 sig
  datatype filtered_value = 
      Literal  of Word16.word (* Literal data for the filter computation. *)
    | Data     of int            (* Use the specified word of the packet. *)
    | Indirect of filtered_value
    | Andb     of filtered_value * filtered_value   (* Bitwise AND. *)
    | Orb      of filtered_value * filtered_value   (* Bitwise OR. *)
    | Xorb     of filtered_value * filtered_value   (* Bitwise XOR. *)
    | Lshift   of filtered_value * int              (* Left Shift. *)
    | Rshift   of filtered_value * int              (* Right Shift. *)
    | Plus     of filtered_value * filtered_value   (* Addition. *)
    | Minus    of filtered_value * filtered_value   (* Subtraction. *)

  datatype filter = 
      True    (* The filter that passes all packets. *)
    | False   (* The filter that passes no packets. *)
    | Equal   of filtered_value * filtered_value   (* Equality. *)
    | Less    of filtered_value * filtered_value   (* Less Than. *)
    | Leq     of filtered_value * filtered_value   (* Less than or equal. *)
    | Greater of filtered_value * filtered_value   (* Greater Than. *)
    | Geq     of filtered_value * filtered_value   (* Greater than or equal. *)
    | Neq     of filtered_value * filtered_value   (* Inequality. *)
        (* When equal: if the two values are the same, stop
	   the computation with true, otherwise continue. *)
    | When_Equal of filtered_value * filtered_value * filter  
        (* Unless equal: if the two values are different, stop
	   the computation with false, otherwise continue. *)
    | Unless_Equal of filtered_value * filtered_value * filter   
        (* When NotEqual: if the two values are not the same, stop
	   the computation with true, otherwise continue. *)
    | When_Not_Equal of filtered_value * filtered_value * filter  
        (* Unless NotEqual: if the two values are not the same, stop
           the computation with false, otherwise continue. *)
    | Unless_Not_Equal of filtered_value * filtered_value * filter

  type packet

  (* compiler *)
  val make: filter -> Word_Array.T

  (* interpreter *)
  val filter: filter -> packet -> bool 

  (* display *)
  val makestring: filter -> string

 end

