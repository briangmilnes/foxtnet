(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edo Biagioni (esb@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

	i.	Abstract

	ipfrag.tst: Test IP fragmentation and reassembly.

	ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor Test_Ip_Frag
	2.	test data
	3.	function check_frag
	4.	function run_tests
	5.	function run
	6.	structure Test_Ip_Frag

	iii.	RCS Log

$Log: ipfrag.tst,v $
Revision 1.4  1996/02/06  23:39:33  esb
adapted to new WORD_ARRAY signature (using words instead of ints).

Revision 1.3  1996/01/15  18:00:49  cline
removed FoxWord

Revision 1.2  1995/08/30  19:37:07  esb
made test programs work.

Revision 1.1  1995/06/29  19:13:54  esb
Initial revision


	1.	functor Test_Ip_Frag
*)

functor Test_Ip_Frag (structure B: FOX_BASIS
		      val debug_level: int ref option): TEST_STRUCTURE =
 struct
  structure Trace = Trace (structure V = B.V
			   val debug_level = debug_level
			   val module_name = "ipfrag.tst"
			   val makestring = fn _ => NONE)

  structure External = Protocol_External (structure B = B
					  val debug_level = debug_level)

  structure Header = Ip_Header (structure In = External
				structure Out = External
				val debug_level = debug_level
				structure B = B)

  structure Fragment = Ip_Fragment (structure Header = Header
				    structure Incoming = External
				    structure Outgoing = External
				    val debug_level = debug_level
				    structure B = B)

(*
	2.	test data
*)

  val data1 = External.uninitialized 0w1
  val data2 = External.uninitialized 0w1000
  val data3 = External.uninitialized 0w2000
  val data4 = External.uninitialized 0w10000
  val data5 = External.uninitialized 0w30000
  val data6 = External.uninitialized 0w65500

(*
	3.	function check_frag
*)

  local
   fun loop_packet (NONE, NONE, _) = true
     | loop_packet (SOME (first1, rest1), SOME (first2, rest2), count) =
        if first1 <> first2 then
	 (Trace.local_print ("byte at position " ^
			     Integer.toString count ^
			     " is 0x" ^
			     Word8.toString first1 ^
			     ", expecting 0x" ^
			     Word8.toString first2);
	  false)
	else
	 loop_packet (Word_Array.W8.Native.F.next rest1,
		      Word_Array.W8.Native.F.next rest2,
		      count + 1)
     | loop_packet _ = false

   fun same_packet (received, sent) () =
        if External.size received <> External.size sent then
	 (Trace.local_print ("sent " ^
			     Word.toString (External.size sent) ^
			     ", received " ^
			     Word.toString (External.size received));
	  false)
	else
	 loop_packet ((Word_Array.W8.Native.F.next o Word_Array.to8)
		      (External.sub
		       (received,
			{start = 0w0, length = External.size received})),
		      (Word_Array.W8.Native.F.next o Word_Array.to8)
		      (External.sub
		       (sent,
			{start = 0w0, length = External.size sent})), 0)

   fun same_header (received, sent) () =
        if Header.equal (received, sent) then true
	else
	 (Trace.local_print ("sent " ^ Header.makestring sent ^
			     ",\nreceived " ^ Header.makestring received);
	  false)

   fun makestring_frag ((fragment, header, header_size), rest) =
        Header.makestring header ^ " (size " ^
        Word.toString header_size ^ "), " ^
        External.makestring_max (fragment, 0w10) ^ " (size " ^
        Word.toString (External.size fragment) ^ "), " ^
	(if rest = "" then rest else "\n" ^ rest)

  in
   fun check_frag (packet, max_packet, min_packet) =
        (let val size = External.size packet
	     val test_name = Word.toString size ^ "/" ^
			     Word.toString min_packet ^ ".." ^
	                     Word.toString max_packet
	     val source = Word32.+ (Word32.<< (Word32.fromInt 0x5432, 0w16),
				    Word32.fromInt 0x1089)
	     val dest = Word32.+ (Word32.<< (Word32.fromInt 0x4321, 0w16),
				  Word32.fromInt 0x0987)
	     val options = [Header.Option.Record_Route
			    (Header.Option.UA
			     {previous = [],
			      available = [Word32.fromInt 0,
					   Word32.fromInt 1]})]
	     val header = Header.V4 {tos = 0w0,
				     data_length = Word16.fromInt
				                     (Word.toInt size),
				     identification = Word16.fromInt 999,
				     flags = Header.Flag.None,
				     ttl = 0w255,
				     protocol = 0w13,
				     source = source,
				     destination = dest,
				     options = options}
	     val frags = Fragment.fragment {packet = packet,
					    header = header,
					    max_size = max_packet,
					    min_size = min_packet}
	     val state = Fragment.new (fn _ => ())
	     fun reassemble (_, result as (SOME _, _)) = result
	       | reassemble ((data, header, hsize), (NONE, state)) =
		  Fragment.reassemble (state, (data, header, packet))
	 in Trace.debug_print (fn _ =>
			       "fragments " ^
			       B.V.List.fold makestring_frag frags "");
	    case B.V.List.fold reassemble frags (NONE, state) of
	       (SOME (received_packet, received_header, _), final_state) =>
		(B.Test.test (test_name ^ " packet",
			      same_packet (received_packet, packet));
		 B.Test.test (test_name ^ " header",
			      same_header (received_header, header)))
	     | (NONE, final_state) =>
		B.Test.test (test_name, fn _ => false)
         end)
	  handle x => (Trace.print_handled (x, NONE);
		       B.Test.test ("exception in fragment/reassembly " ^
				    Word.toString (External.size packet) ^
				    "/" ^ Word.toString min_packet ^ ".." ^
				    Word.toString max_packet,
				    fn _ => false))
  end (* local *)

(*
	4.	function run_tests
*)

  fun run_tests () =
       (check_frag (data1, 0w1500, 0w1);
        check_frag (data2, 0w576, 0w46);
        check_frag (data2, 0w1500, 0w46);
        check_frag (data3, 0w1500, 0w46);
        check_frag (data4, 0w1500, 0w46);
        check_frag (data5, 0w1500, 0w46);
        check_frag (data6, 0w576, 0w46);
        check_frag (data6, 0w1500, 0w46))

(*
	5.	function run
*)

  fun run () = B.Test.tests ("IP fragmentation", 16, run_tests)

  val _ = if ! B.Debug.do_tests then run () else ()

 end (* struct *)

(*
	6.	structure Test_Ip_Frag
*)

structure Test_Ip_Frag = Test_Ip_Frag (structure B = Fox_Basis
				       val debug_level = NONE)






