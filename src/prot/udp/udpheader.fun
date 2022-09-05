(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Ken Cline (Kenneth.Cline@cs.cmu.edu)
	Edoardo Biagioni <Edoardo.Biagioni@cs.cmu.edu>
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

		i.	Abstract

	An implementation of UDP.

		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	signature UDP_HEADER_EXTERN
	2.	functor Udp_Header
	3.	sub-structure Trace

		iii.	RCS Log
	
$Log: udpheader.fun,v $
Revision 1.10  1996/04/18  21:25:23  cline
replaced structures Word16_In and Word16_Out with Word16_Extern

Revision 1.9  1996/02/07  18:58:34  cline
fixed outgoing checksums

Revision 1.8  1996/01/19  23:04:07  esb
adapted to the new wordarray signature.

Revision 1.7  1996/01/15  18:00:49  cline
removed FoxWord

Revision 1.6  1995/11/08  23:10:27  cline
*** empty log message ***

Revision 1.5  1995/08/08  18:23:55  esb
separated in and out external structures.

Revision 1.4  1995/07/04  00:10:48  esb
made checksums work.

Revision 1.3  1995/06/29  19:04:29  cline
fixed type (replaced `length' with `l')

Revision 1.2  1995/06/29  18:47:50  cline
added identify and checksums

Revision 1.1  1995/06/28  20:04:26  cline
Initial revision


		1.	signature UDP_HEADER_EXTERN
*)

signature UDP_HEADER_EXTERN =
 sig
   include EXTERN

  datatype header =
      Incoming_Header of
         {src: Word16.word, dest: Word16.word, data: extern_in}
    | Outgoing_Header of
         {src: Word16.word, dest: Word16.word, data: extern_out,
	  pseudo_header_sum: extern_out -> Word16.word}

   sharing type T = header

   val identify: extern_in * cursor
	       -> {src: Word16.word, dest: Word16.word}
 end

(*
		2.	functor Udp_Header

The UDP header contains the following data (from RFC 0768):

                  0      7 8     15 16    23 24    31  
                 +--------+--------+--------+--------+ 
                 |     Source      |   Destination   | 
                 |      Port       |      Port       | 
                 +--------+--------+--------+--------+ 
                 |                 |                 | 
                 |     Length      |    Checksum     | 
                 +--------+--------+--------+--------+ 
                 |                                     
                 |          data octets ...            
                 +---------------- ...                 

*)

functor Udp_Header (structure In: EXTERNAL
		    structure Out: EXTERNAL
		    structure B: FOX_BASIS
		    val compute_checksums: bool
		    val pseudo_check_in: In.T -> Word16.word
		    val debug_level: int ref option): UDP_HEADER_EXTERN =
 struct

(*
	3.	sub-structure Trace
*)

  structure Trace = Trace (structure V = B.V
			   val debug_level = debug_level
			   val module_name = "udpheader.fun"
			   val makestring = fn _ => NONE)

  structure Word16_Extern =
      Protocol_Extern16_Big (structure In = In
			     structure Out = Out
			     structure B = B)

  type extern_in = In.T
  type extern_out = Out.T

  datatype header =
      Incoming_Header of
         {src: Word16.word, dest: Word16.word, data: extern_in}
    | Outgoing_Header of
         {src: Word16.word, dest: Word16.word, data: extern_out,
	  pseudo_header_sum: extern_out -> Word16.word}

  val fixed_size = 0w8
  fun size _ = fixed_size

  type T = header
  type cursor = Word.word
  exception Extern

  local
   fun makestring_header {src, dest} =
        let val ms = (Integer.toString o Word16.toInt)
	in "source port = " ^ ms src ^
	   ", destination port = " ^ ms dest ^
	   ", data ..."
         end
  in
   fun makestring (Incoming_Header {src, dest, ...}) =
	makestring_header {src = src, dest = dest}
     | makestring (Outgoing_Header {src, dest, ...}) =
	makestring_header {src = src, dest = dest}
  end

  val zero16 = Word16.fromInt 0
  val ffff16 = Word16.fromInt 0xffff

  fun check (src, dest, length, data, data_fold, pseudo_fun) =
       let open B.Checksum
	   val add = one_s_add
	   val pseudoheader_sum = pseudo_fun data
	   val udpheader_sum = add (add (src, dest), length)
	   val data_sum = B.Checksum.complete_partial
	                     (data_fold (data, B.Checksum.check_partial,
					 B.Checksum.initial_state))
	   val initial_sum =
	         B.Checksum.one_s_complement
	           (add (add (pseudoheader_sum, udpheader_sum), data_sum))
       in if initial_sum = zero16 then ffff16 else initial_sum
       end

  fun marshal (array, Outgoing_Header {src, dest, data, pseudo_header_sum}) =
       let val length = Word16.fromInt (Word.toInt
					(Out.size data + fixed_size))
	   val checksum =
	        if compute_checksums then
		  check (src, dest, length, data, Out.fold,
			 (* We must add fixed_size here to include header
			    length in pseudo header checksum calculation *)
			 fn p => B.Checksum.one_s_add
			          (pseudo_header_sum p,
				   Word16.fromInt (Word.toInt fixed_size)))
		else Word16.fromInt 0
       in Word16_Extern.marshal (array, checksum) o
	  Word16_Extern.marshal (array, length) o
	  Word16_Extern.marshal (array, dest) o
	  Word16_Extern.marshal (array, src)
	end
     | marshal _ = raise Extern (* illegal *)

  fun unmarshal (array, cursor) =
       let val (src, cursor) = Word16_Extern.unmarshal (array, cursor)
	   val (dest, cursor) = Word16_Extern.unmarshal (array, cursor)
	   val (length, cursor) = Word16_Extern.unmarshal (array, cursor)
	   val (checksum, cursor) = Word16_Extern.unmarshal (array, cursor)

	   val (header, no_header) =
		 (In.split (array, fixed_size))
	         handle _ =>
		         Trace.print_raise (Extern,
					    SOME "unmarshal, header too small")
	   val (data, _) =
		 (In.split (no_header,
			    Word.fromInt (Word16.toInt length) - 0w8))
	         handle _ =>
		         Trace.print_raise (Extern,
					    SOME "unmarshal, packet too small")

	   val computed_checksum =
	     if checksum = zero16 then zero16
	     else
	        Word16.- (check (src, dest, length, data,
				 In.fold, pseudo_check_in),
			  checksum)
	 in if computed_checksum = zero16 orelse
	       computed_checksum = ffff16 then
	     (Incoming_Header {src = src, dest = dest, data = data}, cursor)
	    else
	     Trace.print_raise (Extern, SOME "unmarshal checksum")
	 end

  fun identify (array, cursor) =
       let val (src, cursor) = Word16_Extern.unmarshal (array, cursor)
	   val (dest, cursor) = Word16_Extern.unmarshal (array, cursor)
       in {src = src, dest = dest}
       end
      handle _ => Trace.print_raise (Extern, SOME "unmarshal, identify")

 end
