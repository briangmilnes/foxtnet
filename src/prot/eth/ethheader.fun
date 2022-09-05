(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Kuo Chiang Chiang (kcchiang@cs.cmu.edu)
	Edoardo Biagioni (Edoardo.Biagioni@cs.cmu.edu)
	Brian Milnes (Brian.Milnes@cs.cmu.edu)
	Ken Cline (Kenneth.Cline@cs.cmu.edu)
        Nick Haines (Nick.Haines@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

	i.	Abstract
		
	This functor marshals and unmarshals Ethernet headers.

	ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor Eth_Header

	iii.	RCS Log
	
$Log: ethheader.fun,v $
Revision 1.6  1997/04/07  12:23:50  esb
added some code to speed up the common case by exposing the wordarray.

Revision 1.5  96/01/19  23:01:26  esb
adapted to the new wordarray signature.

Revision 1.4  1996/01/15  18:00:49  cline
removed FoxWord

Revision 1.3  1995/08/08  18:17:15  esb
split the in and out external structures.

Revision 1.2  1995/06/27  19:00:38  cline
adapted to new extern.sig

Revision 1.1  1995/06/20  16:56:38  esb
Initial revision


	1.	functor Eth_Header
*)

functor Eth_Header (structure In: EXTERNAL
		    structure Out: EXTERNAL
		    structure B: FOX_BASIS): EXTERN =
 struct
  type T = {self: Word48.word, peer: Word48.word, proto: Word16.word}
  type extern_in = In.T
  type extern_out = Out.T
  type cursor = Word.word
  exception Extern

  val fixed_size = 0w14
  fun size _ = fixed_size

  structure Word48X = 
      Protocol_Extern48_Big (structure In = In
			     structure Out = Out
			     structure B = B)
  structure Word16X = 
      Protocol_Extern16_Big (structure In = In
			     structure Out = Out
			     structure B = B)

  fun marshal (array, {self, peer, proto}) =
       (Word16X.marshal (array, proto) o
        Word48X.marshal (array, self) o
        Word48X.marshal (array, peer))

  local
   fun slow_unmarshal (array, cursor) =
        (let val (self, p_cursor) = Word48X.unmarshal (array, cursor)
             val (peer, proto_cursor) = Word48X.unmarshal (array, p_cursor)
             val (proto, last) = Word16X.unmarshal (array, proto_cursor)
         in ({self = self, peer = peer, proto = proto}, last)
         end)
	  handle _ => raise Extern

   fun sub3248 (array, position) =
        Word48.fromLargeWord (Pack32Big.subArr (array, position))

   val proto_mask = Word32.fromInt 0xffff

   fun fast_unmarshal (array, cursor) =
        (let val self_high = sub3248 (array, cursor)
	     val self_low = sub3248 (array, cursor + 1)
	     val peer_high = sub3248 (array, cursor + 2)
	     val peer_proto = Pack32Big.subArr (array, cursor + 3)
	     val peer_low = Word48.fromLargeWord (Word32.>> (peer_proto, 0w16))
	     val proto = Word16.fromLargeWord
	                  (Word32.andb (peer_proto, proto_mask))
	     val self = Word48.orb (Word48.<< (self_high, 0w32), self_low)
	     val peer = Word48.orb (Word48.<< (peer_high, 0w16), peer_low)
         in {self = self, peer = peer, proto = proto}
         end)
	  handle _ => raise Extern

  in
   fun unmarshal (args as (array, cursor)) =
        if cursor <> 0w0 then slow_unmarshal args
        else
	 case In.fold (array, fn (wa, l) => wa :: l, []) of
	    [singleton] =>
	     let val (array, first, last) = Word_Array.expose singleton
	     in if Word.mod (first, 0w4) = 0w2 then
		 (fast_unmarshal (array, Word.toInt first div 4),
		  cursor + fixed_size)
		else slow_unmarshal args
	     end
	  | _ => slow_unmarshal args
  end

 end (* struct *)
