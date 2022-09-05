(*

        FoxNet: The Fox Project's Communication Protocol Implementation Effort
        Edo Biagioni (esb@cs.cmu.edu)
	Ken Cline (Ken.Cline@cs.cmu.edu)
        Nick Haines (nickh@cs.cmu.edu)
        Brian Milnes (milnes@cs.cmu.edu)
        Fox Project
        School of Computer Science
        Carnegie Mellon University
        Pittsburgh, Pa 15139-3891

	i.	Abstract

	extern32.fun: extern structures for Word32s.

	ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor Extern_String_Length
	2.	functor Extern_C_String

	iii.	RCS Log

$Log: externstring.fun,v $
Revision 1.7  1996/04/18  18:45:35  cline
added Extern_C_String

Revision 1.6  1996/02/06  23:44:11  esb
adapted to new wordarray.

Revision 1.5  1996/01/19  23:10:20  esb
adapted to the new wordarray signature.

Revision 1.4  1996/01/15  18:00:49  cline
removed FoxWord

Revision 1.3  1995/11/12  16:51:53  esb
adapted to new Word_Array.

Revision 1.2  1995/10/12  17:49:40  cline
fixed typo in comment

Revision 1.1  1995/10/12  14:21:02  cline
Initial revision



		1.	functor Extern_String_Length
*)

functor Extern_String_Length (structure In: EXTERNAL
			      structure Out: EXTERNAL
			      structure V: VENDOR): EXTERN =
  struct
    type T = string
    type extern_in = In.T * Word.word (* string length *)
    type extern_out = Out.T
    type cursor = Word.word
    exception Extern

    val size = Word.fromInt o V.String.length

    fun marshal (extern, s) cursor =
         let val size_s = size s
	     fun sub i = (Word8.fromInt o V.Char.ord)
	                 (V.String.ordof (s, Word.toInt i))
	     val w8 = Word_Array.W8.U_Big.F.tabulate (sub, size_s)
	 in Out.update (extern, cursor, Word_Array.from8 w8);
	    cursor + size_s
	 end

    fun unmarshal ((extern, len), cursor) =
         let val w8 = (In.sub (extern, {start = cursor, length = len}))
	              handle _ => raise Extern
	     fun loop x =
	          case Word_Array.W8.U_Big.F.next x of
		     SOME (w, x') =>
		      V.Char.chr (Word8.toInt w) :: loop x'
		   | NONE => []
	 in (V.String.implode (loop (Word_Array.to8 w8)), cursor + len)
	 end

  end (* struct *)

(*
		2.	functor Extern_C_String
*)

functor Extern_C_String (structure In: EXTERNAL
			 structure Out: EXTERNAL
			 structure V: VENDOR): EXTERN =
  struct
    type T = string
    type extern_in = In.T
    type extern_out = Out.T
    type cursor = Word.word
    exception Extern

    fun size s = Word.fromInt (V.String.length s + 1)

    fun marshal (extern, s) cursor =
         let val size_s = size s
	     fun sub i = (Word8.fromInt o V.Char.ord)
	                 (V.String.ordof (s^"\000", Word.toInt i))
	     val w8 = Word_Array.W8.U_Big.F.tabulate (sub, size_s)
	 in Out.update (extern, cursor, Word_Array.from8 w8);

	    cursor + size_s
	 end

    fun unmarshal (extern, cursor) =
         let val array = Word_Array.to8
			 (In.sub (extern, {start = cursor,
					   length = In.size extern - cursor}))
	     fun add (ch, (l, i)) = (ch::l, i)
	     fun loop i =
	       case Word_Array.W8.Native.F.nth (array, i) of
		 0w0 => ([], i+0w1)
	       | c => add (V.Char.chr (Word8.toInt c), loop (i+0w1))
	     fun finish (l, i) = (V.String.implode l, cursor+i)
	 in finish (loop 0w0)
	 end

  end (* struct *)
