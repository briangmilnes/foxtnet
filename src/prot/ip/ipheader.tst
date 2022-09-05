(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edo Biagioni (esb@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

	i.	Abstract

	ipheader.tst: Test IP header un/marshaling.

	ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor Test_Ip_Header
	2.	test data
	3.	function check_header
	4.	function run_tests
	5.	function run

	iii.	RCS Log

$Log: ipheader.tst,v $
Revision 1.4  1996/02/06  23:39:33  esb
adapted to new WORD_ARRAY signature (using words instead of ints).

Revision 1.3  1996/01/15  18:00:49  cline
removed FoxWord

Revision 1.2  1995/08/30  19:37:07  esb
made test programs work.

Revision 1.1  1995/06/23  20:13:38  esb
Initial revision


	1.	functor Test_Ip_Header
*)

functor Test_Ip_Header (structure B: FOX_BASIS
			val debug_level: int ref option): TEST_STRUCTURE =
 struct
  structure Trace = Trace (structure V = B.V
			   val debug_level = debug_level
			   val module_name = "ipheader.tst"
			   val makestring = fn _ => NONE)

  structure External = Protocol_External (structure B = B
					  val debug_level = debug_level)

  structure Header = Ip_Header (structure Out = External
				structure In = External
				val debug_level = debug_level
				structure B = B)

(*
	2.	test data
*)

  fun round_up (value, step) =
       if value mod step = 0 then value
       else value + (step - value mod step)

  fun round_header value = Word.fromInt (round_up (value, 4))

  fun stringToWord32 s =
    let exception Word32_Syntax in
      case Word32.fromString s of
	SOME w => w
      | NONE => raise Word32_Syntax
    end

  val header1 = Header.V4 {tos = 0w0,
			   data_length = Word16.fromInt 3,
			   identification = Word16.fromInt 0x999,
			   flags = Header.Flag.None,
			   ttl = 0w64,
			   protocol = 0w6,
			   source = stringToWord32  "12345678",
			   destination = stringToWord32 "98765432",
			   options = []}
  val header1_size = round_header 20

  val header2 =
       Header.V4 {tos = 0w0,
		  data_length = Word16.fromInt 1024,
		  identification = Word16.fromInt 0x998,
		  flags = Header.Flag.Dont_Fragment,
		  ttl = 0w1,
		  protocol = 0w17,
		  source = stringToWord32 "23456789",
		  destination = stringToWord32 "87654321",
		  options = [Header.Option.Loose_Route
			     (Header.Option.UA
			      {previous = [],
			       available = [Word32.fromInt 0,
					    Word32.fromInt 1,
					    Word32.fromInt 2,
					    Word32.fromInt 3]}),
				      Header.Option.Time_Stamp
				       {stamps = 
					 Header.Option.Stamp
					 (Header.Option.UA
					  {previous = [], available = []}),
					overflow = 0}]}
  val header2' =
       Header.V4 {tos = 0w0,
		  data_length = Word16.fromInt 1024,
		  identification = Word16.fromInt 0x998,
		  flags = Header.Flag.Dont_Fragment,
		  ttl = 0w1,
		  protocol = 0w17,
		  source = stringToWord32 "23456789",
		  destination = stringToWord32 "87654321",
		  options = [Header.Option.Loose_Route
			     (Header.Option.UA
			      {previous = [stringToWord32 "87654321"],
			       available = [Word32.fromInt 1,
					    Word32.fromInt 2,
					     Word32.fromInt 3]}),
			     Header.Option.Time_Stamp
			     {stamps = 
			      Header.Option.Stamp
			      (Header.Option.UA
			       {previous = [], available = []}),
			      overflow = 0}]}
  val header2_size = round_header (20 + (3 + 4 * 4) + (4 + 0 * 4))

  val header3 =
       Header.V4 {tos = 0w1,
		  data_length = Word16.fromInt 0xff00,
		  identification = Word16.fromInt 0x997,
		  flags = Header.Flag.Fragment (Word16.fromInt 0x123),
		  ttl = 0w255,
		  protocol = 0w255,
		  source = stringToWord32 "34567890",
		  destination = stringToWord32 "76543210",
		  options = [Header.Option.Time_Stamp
			     {stamps = 
			      Header.Option.Record_Stamp
			      (Header.Option.UA
			       {previous = [{ip = Word32.fromInt 9,
					     time = Word32.fromInt 10}],
				available = [{ip = Word32.fromInt 13,
					      time = Word32.fromInt 19},
					     {ip = Word32.fromInt 14,
					      time = Word32.fromInt 18},
					     {ip = Word32.fromInt 15,
					      time = Word32.fromInt 17}]}),
			      overflow = 0}]}
  val header3_size = round_header (20 + (4 + 4 * 8))

  val header4 =
       Header.V4 {tos = 0w1,
		  data_length = Word16.fromInt 0x1,
		  identification = Word16.fromInt 0x996,
		  flags = Header.Flag.Last_Fragment (Word16.fromInt 0x123),
		  ttl = 0w4,
		  protocol = 0w1,
		  source = stringToWord32 "45678901",
		  destination = stringToWord32 "65432109",
		  options = [Header.Option.Time_Stamp
			     {stamps = 
			      Header.Option.Given_Stamp
			      (Header.Option.UA
			       {previous = [],
				available = [{ip = Word32.fromInt 16,
					      time = Word32.fromInt 17}]}),
			      overflow = 3},
			     Header.Option.Other_Option
			     {option_type = 0wx18,
			      send_in_fragments = false,
			      contents = Word_Array.from8
			      (Word_Array.W8.Native.F.create (0w0, 0w10))}]}
  val header4_size = round_header (20 + (4 + 1 * 8) + (2 + 10))

  val header5 =
       Header.V4 {tos = 0w2,
		  data_length = Word16.fromInt 999,
		  identification = Word16.fromInt 0x995,
		  flags = Header.Flag.None,
		  ttl = 0w77,
		  protocol = 0w4,
		  source = stringToWord32 "56789012",
		  destination = stringToWord32 "54321098",
		  options = [Header.Option.Strict_Route
			     (Header.Option.UA
			      {previous = [Word32.fromInt 4],
			       available = [Word32.fromInt 5]}),
			     Header.Option.Record_Route
			     (Header.Option.UA
			      {previous = [Word32.fromInt 6, Word32.fromInt 7],
			       available = []})]}
  val header5' =
       Header.V4 {tos = 0w2,
		  data_length = Word16.fromInt 999,
		  identification = Word16.fromInt 0x995,
		  flags = Header.Flag.None,
		  ttl = 0w77,
		  protocol = 0w4,
		  source = stringToWord32 "56789012",
		  destination = stringToWord32 "54321098",
		  options = [Header.Option.Strict_Route
			     (Header.Option.UA
			      {previous = [Word32.fromInt 4,
					   stringToWord32 "54321098"],
			       available = []}),
			     Header.Option.Record_Route
			     (Header.Option.UA
			      {previous = [Word32.fromInt 6, Word32.fromInt 7],
			       available = []})]}
  val header5_size = round_header (20 + (3 + 2 * 4) + (3 + 2 * 4))


(*
	3.	function check_header
*)

  local
   fun check_word (header, value, expected, name) =
        if value <> expected then
	 (Trace.local_print (name ^ " is " ^ Word.toString value ^
			     " instead of " ^ Word.toString expected ^
			     " for header " ^ Header.makestring header);
	  B.Test.test (name, fn _ => false))
        else
	 B.Test.test (name, fn _ => true)

   fun check_same_header (header, expected, name) =
        if Header.equal (header, expected) then
	 B.Test.test (name, fn _ => true)
	else
	 (Trace.local_print (name ^ " gives " ^ Header.makestring header ^
			     " instead of " ^ Header.makestring expected);
	  B.Test.test (name, fn _ => false))

  in
   fun check_header (header, result_header, header_size, version) =
        (let val version_name = " " ^ Integer.toString version
	     val computed_size = Header.size header
	     val _ = check_word (header, computed_size, header_size,
				 "computed size" ^ version_name)
	     val external = External.uninitialized computed_size
	     val cursor = Header.marshal (external, header) 0w0
	     val _ = check_word (header, cursor, header_size,
				 "marshal cursor" ^ version_name)
	     val (result, result_cursor) = Header.unmarshal (external, 0w0)
         in check_word (header, result_cursor, header_size,
			"unmarshal cursor" ^ version_name);
	    check_same_header (result_header, result,
			       "un/marshal" ^ version_name)
         end)
	  handle x => (Trace.print_handled (x, NONE);
		       B.Test.test ("exception in un/marshal " ^
				    Integer.toString version, fn _ => false))
  end (* local *)

(*
	4.	function run_tests
*)

  fun run_tests () =
       (check_header (header1, header1, header1_size, 1);
        check_header (header2, header2', header2_size, 2);
        check_header (header3, header3, header3_size, 3);
        check_header (header4, header4, header4_size, 4);
        check_header (header5, header5', header5_size, 5))

(*
	5.	function run
*)

  fun run () = B.Test.tests ("IP header", 20, run_tests)

  val _ = if ! B.Debug.do_tests then run () else ()

 end (* struct *)

(*
	.	structure Test_Ip_Header
*)

structure Test_Ip_Header = Test_Ip_Header (structure B = Fox_Basis
					   val debug_level = NONE)






