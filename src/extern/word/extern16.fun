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

	extern16.fun: extern structures for Word16s.

	ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor Protocol_Extern16_Big
	2.	functor Protocol_Extern16_Little

	iii.	RCS Log

$Log: extern16.fun,v $
Revision 1.4  1995/08/08  18:27:24  esb
separated input and output external structures.

Revision 1.3  1995/06/29  18:21:23  esb
adapted to new wordarray.

Revision 1.2  1995/06/27  18:59:29  cline
adapted to new extern.sig

Revision 1.1  1995/06/20  17:43:24  esb
Initial revision


		1.	functor Protocol_Extern16_Big
*)

functor Protocol_Extern16_Big (structure In: EXTERNAL
			       structure Out: EXTERNAL
			       structure B: FOX_BASIS): EXTERN_KEY =
 struct
  type T = FoxWord16.word
  type extern_in = In.T
  type extern_out = Out.T
  type cursor = int
  exception Extern

  val constant_size = 2
  fun size _ = constant_size

  fun marshal (extern, value) cursor =
       let val w16 = Word_Array.W16.Big.create (value, 1)
	   val w8 = Word_Array.convert8 (Word_Array.Array16 w16)
       in Out.update (extern, cursor, w8);
	  cursor + constant_size
       end

  fun unmarshal (extern, cursor) =
       let val w8 = In.sub (extern, {start = cursor,
				     length = constant_size})
	   val (w16, _) = (Word_Array.convert16 o Word_Array.Array8) w8
       in case Word_Array.W16.Big.next w16 of
	     NONE => raise Extern
	   | SOME (value, _) => (value, cursor + constant_size)
       end

  fun makestring n =
       let fun makestring_one byte = FoxMakestring.word16 byte
	   val mask = SW.n16 "0xff"
	   fun make_byte shift =
	        FoxWord16.andb (FoxWord16.rshiftl (n, shift), mask)
	   val b0 = makestring_one (make_byte  8)
	   val b1 = makestring_one (make_byte  0)
	 in b0 ^ "." ^ b1
	 end

  fun equal (a: FoxWord16.word, b) = a = b
  val hash = FoxWord16.wordToInt

 end

(*
		2.	functor Protocol_Extern16_Little
*)

functor Protocol_Extern16_Little (
structure In: EXTERNAL
				  structure Out: EXTERNAL
			          structure B: FOX_BASIS): EXTERN_KEY =
 struct
  type T = FoxWord16.word
  type extern_in = In.T
  type extern_out = Out.T
  type cursor = int
  exception Extern

  val constant_size = 2
  fun size _ = constant_size

  fun marshal (extern, value) cursor =
       let val w16 = Word_Array.W16.Little.create (value, 1)
	   val w8 = Word_Array.convert8 (Word_Array.Array16 w16)
       in Out.update (extern, cursor, w8);
	  cursor + constant_size
       end

  fun unmarshal (extern, cursor) =
       let val w8 = In.sub (extern, {start = cursor, length = 1})
	   val (w16, _) = Word_Array.convert16 (Word_Array.Array8 w8)
       in case Word_Array.W16.Little.next w16 of
	     NONE => raise Extern
	   | SOME (value, _) => (value, cursor + constant_size)
       end

  fun makestring n =
       let fun makestring_one byte = FoxMakestring.word16 byte
	   val mask = SW.n16 "0xff"
	   fun make_byte shift =
	        FoxWord16.andb (FoxWord16.rshiftl (n, shift), mask)
	   val b0 = makestring_one (make_byte  8)
	   val b1 = makestring_one (make_byte  0)
	 in b0 ^ "." ^ b1
	 end

  fun equal (a: FoxWord16.word, b) = a = b
  val hash = FoxWord16.wordToInt

 end
