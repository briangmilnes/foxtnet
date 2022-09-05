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
	1.	functor Protocol_Extern32_Big
	2.	functor Protocol_Extern32_Little

	iii.	RCS Log

$Log: extern32.fun,v $
Revision 1.5  1995/08/08  18:27:24  esb
separated input and output external structures.

Revision 1.4  1995/06/29  18:21:23  esb
adapted to new wordarray.

Revision 1.3  1995/06/27  18:59:40  cline
adapted to new extern.sig

Revision 1.2  1995/06/23  19:58:00  esb
bug fix.

Revision 1.1  1995/06/20  17:43:24  esb
Initial revision


		1.	functor Protocol_Extern32_Big
*)

functor Protocol_Extern32_Big (structure In: EXTERNAL
			       structure Out: EXTERNAL
			       structure B: FOX_BASIS): EXTERN_KEY =
 struct
  type T = FoxWord32.word
  type extern_in = In.T
  type extern_out = Out.T
  type cursor = int
  exception Extern

  val constant_size = 4
  fun size _ = constant_size

  fun marshal (extern, value) cursor =
       let val w32 = Word_Array.W32.Big.create (value, 1)
	   val w8 = Word_Array.convert8 (Word_Array.Array32 w32)
       in Out.update (extern, cursor, w8);
	  cursor + constant_size
       end

  fun unmarshal (extern, cursor) =
       let val w8 = In.sub (extern, {start = cursor,
				     length = constant_size})
	   val (w32, _) = Word_Array.convert32 (Word_Array.Array8 w8)
       in case Word_Array.W32.Big.next w32 of
	     NONE => raise Extern
	   | SOME (value, _) => (value, cursor + constant_size)
       end

  fun makestring n =
       let fun makestring_one byte = FoxMakestring.word32 byte
	   val mask = SW.n32 "0xff"
	   fun make_byte shift =
	        FoxWord32.andb (FoxWord32.rshiftl (n, shift), mask)
	   val b0 = makestring_one (make_byte 24)
	   val b1 = makestring_one (make_byte 16)
	   val b2 = makestring_one (make_byte  8)
	   val b3 = makestring_one (make_byte  0)
	 in b0 ^ "." ^ b1 ^ "." ^ b2 ^ "." ^ b3
	 end

  fun equal (a: FoxWord32.word, b) = a = b
  val hash = FoxWord32.wordToInt

 end

(*
		2.	functor Protocol_Extern32_Little
*)

functor Protocol_Extern32_Little (structure In: EXTERNAL
				  structure Out: EXTERNAL
			          structure B: FOX_BASIS): EXTERN_KEY =
 struct
  type T = FoxWord32.word
  type extern_in = In.T
  type extern_out = Out.T
  type cursor = int
  exception Extern

  val constant_size = 4
  fun size _ = constant_size

  fun marshal (extern, value) cursor =
       let val w32 = Word_Array.W32.Little.create (value, 1)
	   val w8 = Word_Array.convert8 (Word_Array.Array32 w32)
       in Out.update (extern, cursor, w8);
	  cursor + constant_size
       end

  fun unmarshal (extern, cursor) =
       let val w8 = In.sub (extern, {start = cursor,
				     length = constant_size})
	   val (w32, _) = Word_Array.convert32 (Word_Array.Array8 w8)
       in case Word_Array.W32.Little.next w32 of
	     NONE => raise Extern
	   | SOME (value, _) => (value, cursor + constant_size)
       end

  fun makestring n =
       let fun makestring_one byte = FoxMakestring.word32 byte
	   val mask = SW.n32 "0xff"
	   fun make_byte shift =
	        FoxWord32.andb (FoxWord32.rshiftl (n, shift), mask)
	   val b0 = makestring_one (make_byte 24)
	   val b1 = makestring_one (make_byte 16)
	   val b2 = makestring_one (make_byte  8)
	   val b3 = makestring_one (make_byte  0)
	 in b0 ^ "." ^ b1 ^ "." ^ b2 ^ "." ^ b3
	 end

  fun equal (a: FoxWord32.word, b) = a = b
  val hash = FoxWord32.wordToInt

 end
