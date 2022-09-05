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

	This marshalls null terminated strings.

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor CString

		iii.	RCS Log
	
$Log: cstring.fun,v $
Revision 1.10  1996/01/19  23:09:46  esb
adapted to the new wordarray signature.

Revision 1.9  1996/01/15  18:00:49  cline
removed FoxWord

Revision 1.8  1995/06/27  18:57:42  cline
adapted to new extern.sig

Revision 1.7  1995/06/20  17:46:52  esb
minor fix.

Revision 1.6  1995/03/12  17:59:54  esb
adapted to new trace.sig.

Revision 1.5  1995/03/10  03:53:53  esb
adapted to new vendor.sig.

Revision 1.4  1994/11/09  20:49:09  esb
adapted to new extern.sig.

Revision 1.3  1994/09/30  16:55:23  esb
replaced ubytes by fox words.

Revision 1.2  1994/08/25  23:47:51  robby
updated for new basis

Revision 1.1  1994/07/14  20:28:28  robby
Initial revision

Revision 1.1  94/07/13  18:44:12  robby
Initial revision


		1.	functor CString
*)

functor CString (structure Access: ACCESS
		 structure V: VENDOR
		 val debug_level: int ref option): STRING_LINEARIZE = 
 struct
  exception Extern
  fun makestring_exn Extern = SOME "cstring.fun: Extern"
    | makestring_exn _ = NONE

  structure Trace = Trace (structure V = V
			   val debug_level = debug_level
			   val module_name = "cstring.fun"
			   val makestring = makestring_exn)
  val local_print = Trace.local_print
  val debug_print = Trace.debug_print

  type T = string
  type extern_in = Word8Array.array
  type extern_out = Word8Array.array
  type cursor = Word.word

  fun size s = Word.fromInt (V.String.length s) + 0w1

  fun marshal (array, string) pos = 
       (Access.string_to_range (array, Word.toInt pos, string);
	Word8Array.update (array, Word.toInt pos + V.String.length string,
			   0w0);
	pos + size string)
       handle Access.Bad_Range => raise Extern
	    | Subscript => raise Extern

  val zero_byte = Word8.fromInt 0

  fun unmarshal (array, pos) = 
       let fun zerop x = x = zero_byte
           val range = Access.Test {first = Word.toInt pos, test = zerop}
	   val string = Access.range_to_string (array, range)
       in (string, pos + (size string))
       end
      handle Access.Bad_Range => raise Extern
 end
