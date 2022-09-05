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

	extern64.fun: extern structures for Word64s.

	ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor Protocol_Extern64_Big
	2.	functor Protocol_Extern64_Little

	iii.	RCS Log

$Log: extern64.fun,v $
Revision 1.5  1995/08/08  18:27:24  esb
separated input and output external structures.

Revision 1.4  1995/06/29  18:21:23  esb
adapted to new wordarray.

Revision 1.3  1995/06/27  19:00:07  cline
adapted to new extern.sig

Revision 1.2  1995/06/23  19:58:00  esb
bug fix.

Revision 1.1  1995/06/20  17:43:24  esb
Initial revision


		1.	functor Protocol_Extern64_Big
*)

functor Protocol_Extern64_Big (structure In: EXTERNAL
			       structure Out: EXTERNAL
			       structure B: FOX_BASIS): EXTERN_KEY =
 struct
  type T = FoxWord64.word
  type extern_in = In.T
  type extern_out = Out.T
  type cursor = int
  exception Extern

  val constant_size = 8
  fun size _ = constant_size

  fun marshal (extern, value) cursor =
       let val w64 = Word_Array.W64.Big.create (value, 1)
	   val w8 = Word_Array.convert8 (Word_Array.Array64 w64)
       in Out.update (extern, cursor, w8);
	  cursor + constant_size
       end

  fun unmarshal (extern, cursor) =
       let val w8 = In.sub (extern, {start = cursor,
				     length = constant_size})
	   val (w64, _) = Word_Array.convert64 (Word_Array.Array8 w8)
       in case Word_Array.W64.Big.next w64 of
	     NONE => raise Extern
	   | SOME (value, _) => (value, cursor + constant_size)
       end

  fun makestring n =
       let fun makestring_one byte = FoxMakestring.word64 byte
	   val mask = SW.n64 "0xff"
	   fun make_byte shift =
	        FoxWord64.andb (FoxWord64.rshiftl (n, shift), mask)
	   val b0 = makestring_one (make_byte 56)
	   val b1 = makestring_one (make_byte 48)
	   val b2 = makestring_one (make_byte 40)
	   val b3 = makestring_one (make_byte 32)
	   val b4 = makestring_one (make_byte 24)
	   val b5 = makestring_one (make_byte 16)
	   val b6 = makestring_one (make_byte  8)
	   val b7 = makestring_one (make_byte  0)
	 in b0 ^ "." ^ b1 ^ "." ^ b2 ^ "." ^ b3 ^ "." ^
	    b4 ^ "." ^ b5 ^ "." ^ b6 ^ "." ^ b7
	 end

  fun equal (a: FoxWord64.word, b) = a = b
  val hash = FoxWord64.wordToInt

 end

(*
		2.	functor Protocol_Extern64_Little
*)

functor Protocol_Extern64_Little (structure In: EXTERNAL
				  structure Out: EXTERNAL
			          structure B: FOX_BASIS): EXTERN_KEY =
 struct
  type T = FoxWord64.word
  type extern_in = In.T
  type extern_out = Out.T
  type cursor = int
  exception Extern

  val constant_size = 8
  fun size _ = constant_size

  fun marshal (extern, value) cursor =
       let val w64 = Word_Array.W64.Little.create (value, 1)
	   val w8 = Word_Array.convert8 (Word_Array.Array64 w64)
       in Out.update (extern, cursor, w8);
	  cursor + constant_size
       end

  fun unmarshal (extern, cursor) =
       let val w8 = In.sub (extern, {start = cursor,
				     length = constant_size})
	   val (w64, _) = Word_Array.convert64 (Word_Array.Array8 w8)
       in case Word_Array.W64.Little.next w64 of
	     NONE => raise Extern
	   | SOME (value, _) => (value, cursor + constant_size)
       end

  fun makestring n =
       let fun makestring_one byte = FoxMakestring.word64 byte
	   val mask = SW.n64 "0xff"
	   fun make_byte shift =
	        FoxWord64.andb (FoxWord64.rshiftl (n, shift), mask)
	   val b0 = makestring_one (make_byte 56)
	   val b1 = makestring_one (make_byte 48)
	   val b2 = makestring_one (make_byte 40)
	   val b3 = makestring_one (make_byte 32)
	   val b4 = makestring_one (make_byte 24)
	   val b5 = makestring_one (make_byte 16)
	   val b6 = makestring_one (make_byte  8)
	   val b7 = makestring_one (make_byte  0)
	 in b0 ^ "." ^ b1 ^ "." ^ b2 ^ "." ^ b3 ^ "." ^
	    b4 ^ "." ^ b5 ^ "." ^ b6 ^ "." ^ b7
	 end

  fun equal (a: FoxWord64.word, b) = a = b
  val hash = FoxWord64.wordToInt

 end
