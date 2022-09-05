(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edo Biagioni (esb@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

	i.	Abstract

	icmpheader.tst: Test IP header un/marshaling.

	ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor Test_Icmp_Header
	2.	test data
	3.	function check_header
	4.	function run_tests
	5.	function run
	6.	structure Test_Icmp_Header

	iii.	RCS Log

$Log: icmpheader.tst,v $
Revision 1.3  1995/09/26  16:29:08  esb
added router advertisement messages and obsolete for obsolete messages.

Revision 1.2  1995/08/30  19:37:44  esb
made test program work.

Revision 1.1  1995/08/08  18:25:51  esb
Initial revision


	1.	functor Test_Icmp_Header
*)

functor Test_Icmp_Header (structure B: FOX_BASIS
			  val debug_level: int ref option): TEST_STRUCTURE =
 struct
  structure Trace = Trace (structure V = B.V
			   val debug_level = debug_level
			   val module_name = "icmpheader.tst"
			   val makestring = fn _ => NONE)

  structure Trace_Icmp = Trace (structure V = B.V
				val debug_level = debug_level
				val module_name = "icmpheader.fun"
				val makestring = fn _ => NONE)

  structure External = Protocol_External (structure B = B
					  val debug_level = debug_level)

  structure Ip_Header = Ip_Header (structure In = External
				   structure Out = External
				   val debug_level = debug_level
				   structure B = B)

  structure Ip_Extern =
   struct
    structure Word32 = 
        Protocol_Extern32_Big (structure In = External
			       structure Out = External
			       structure B = B)
    open Word32
    val makestring = FoxMakestring.word32
    val equal = op=
    val hash = FoxWord32.wordToInt
   end

  structure R = Icmp_Redirect ()
  structure U = Icmp_Unreachable ()

  structure In = Icmp_In (structure In = External
			  structure Ip_Header = Ip_Header
			  structure Ip_Extern = Ip_Extern
			  structure Redirect = R
			  structure Unreachable = U
			  structure Trace = Trace_Icmp
			  structure B = B
			  val debug_level = debug_level)

  structure Out = Icmp_Out (structure Out = External
			    structure Ip_Extern = Ip_Extern
			    structure Redirect = R
			    structure Unreachable = U
			    structure Trace = Trace_Icmp
			    structure B = B
			    val debug_level = debug_level)

(*
	2.	test data
*)

  local
   val dest_ip = SW.n32 "0x12345678"
   fun ip_header_data_options (selector, options) =
        (Ip_Header.V4 {tos = SW.n8 "0", data_length = SW.n16 "99",
		       identification = SW.n16 "88",
		       flags = Ip_Header.Flag.None, ttl = SW.n8 "77",
		       protocol = FoxWord8.intToWord selector,
		       source = FoxWord32.intToWord selector,
		       destination = dest_ip,
		       options = options},
         Word_Array.W8.tabulate (8, fn x => FoxWord8.intToWord (selector + x)))

   fun marshal_error (x, lin_header, header) =
        let val name = "Ip_Header.marshal ("
	    val size = B.V.Integer.makestring (External.size lin_header)
	    val needed = " (needed "
	    val expected = B.V.Integer.makestring (Ip_Header.size header)
	    val head = "), " ^ Ip_Header.makestring header
	    val close = ")"
	    val location = name ^ size ^ needed ^ expected ^ head ^ close
	in Trace.print_raise_again (x, SOME location)
	end

   fun ip_send_options (selector, options) =
        let val (header, data) = ip_header_data_options (selector, options)
	    val lin_header = External.uninitialized (Ip_Header.size header)
	    val lin_data = External.new data
        in ((Ip_Header.marshal (lin_header, header) 0)
	    handle x => marshal_error (x, lin_header, header));
	   External.join (lin_header, lin_data)
        end

   fun ip_send selector = ip_send_options (selector, [])

   fun ip_recv_options (selector, options) =
        let val (header, data) = ip_header_data_options (selector, options)
	    val lin_header = External.uninitialized (Ip_Header.size header)
	    val lin_data = External.new data
        in ((Ip_Header.marshal (lin_header, header) 0)
	    handle x => marshal_error (x, lin_header, header));
	   External.join (lin_header, lin_data)
        end

   fun ip_recv selector = ip_recv_options (selector, [])

   val option1 = Ip_Header.Option.Loose_Route (* 19 bytes *)
                  (Ip_Header.Option.UA {previous = [SW.n32 "2", SW.n32 "3"],
					available = [SW.n32 "0", SW.n32 "1"]})
   val option1_recv =
         Ip_Header.Option.Loose_Route (* 19 bytes *)
	    (Ip_Header.Option.UA {previous = [SW.n32 "2", SW.n32 "3", dest_ip],
				  available = [SW.n32 "1"]})
   val option2 = Ip_Header.Option.Other_Option (* 5 bytes *)
                  {option_type = SW.n8 "31", send_in_fragments = true,
		   contents = Word_Array.W8.create (SW.n8 "7", 3)}
   val option3 = Ip_Header.Option.Strict_Route (* 11 bytes *)
                  (Ip_Header.Option.UA {previous = [],
					available = [SW.n32 "2", SW.n32 "3"]})
   val option3_recv =
        Ip_Header.Option.Strict_Route (* 11 bytes *)
              (Ip_Header.Option.UA {previous = [dest_ip],
				    available = [SW.n32 "3"]})
   val all_options = [option1, option2, option3]
  in
   val tests =
        [(Out.Unreachable (U.Network_Unreachable, ip_send 1),
	  In.Unreachable (U.Network_Unreachable, ip_recv 1)),
	 (Out.Unreachable (U.Host_Unreachable, ip_send 2),
	  In.Unreachable (U.Host_Unreachable, ip_recv 2)),
	 (Out.Unreachable (U.Protocol_Unreachable, ip_send 3),
	  In.Unreachable (U.Protocol_Unreachable, ip_recv 3)),
	 (Out.Unreachable (U.Port_Unreachable, ip_send 4),
	  In.Unreachable (U.Port_Unreachable, ip_recv 4)),
	 (Out.Unreachable (U.Fragmentation_Needed {mtu = SW.n16 "999"},
			   ip_send 5),
	  In.Unreachable (U.Fragmentation_Needed {mtu = SW.n16 "999"},
			  ip_recv 5)),
	 (Out.Unreachable (U.Source_Route_Failed, ip_send 6),
	  In.Unreachable (U.Source_Route_Failed, ip_recv 6)),
	 (Out.Unreachable (U.Network_Unknown, ip_send 7),
	  In.Unreachable (U.Network_Unknown, ip_recv 7)),
	 (Out.Unreachable (U.Host_Unknown, ip_send 8),
	  In.Unreachable (U.Host_Unknown, ip_recv 8)),
	 (Out.Unreachable (U.Source_Host_Isolated, ip_send 9),
	  In.Unreachable (U.Source_Host_Isolated, ip_recv 9)),
	 (Out.Unreachable (U.Communication_With_Network_Prohibited,
			   ip_send 10),
	  In.Unreachable (U.Communication_With_Network_Prohibited,
			  ip_recv 10)),
	 (Out.Unreachable (U.Communication_With_Host_Prohibited, ip_send 11),
	  In.Unreachable (U.Communication_With_Host_Prohibited, ip_recv 11)),
	 (Out.Unreachable (U.Network_Unreachable_For_Tos, ip_send 12),
	  In.Unreachable (U.Network_Unreachable_For_Tos, ip_recv 12)),
	 (Out.Unreachable (U.Host_Unreachable_For_Tos, ip_send 13),
	  In.Unreachable (U.Host_Unreachable_For_Tos, ip_recv 13)),
	 (Out.Transit_Time_Exceeded (ip_send 14),
	  In.Transit_Time_Exceeded (ip_recv 14)),
	 (Out.Reassembly_Time_Exceeded (ip_send 15),
	  In.Reassembly_Time_Exceeded (ip_recv 15)),
	 (Out.Parameter_Problem {pointer = SW.n8 "2", data = ip_send 16},
	  In.Parameter_Problem (In.Header (SW.n8 "2"), ip_recv 16)),
	 (Out.Parameter_Problem {pointer = SW.n8 "22",
				 data = ip_send_options (17, all_options)},
	  In.Parameter_Problem (In.Option option1_recv,
				ip_recv_options (17, all_options))),
	 (Out.Parameter_Problem {pointer = SW.n8 "39",
				 data = ip_send_options (18, all_options)},
	  In.Parameter_Problem (In.Option option2,
				ip_recv_options (18, all_options))),
	 (Out.Parameter_Problem {pointer = SW.n8 "54",
				 data = ip_send_options (19, all_options)},
	  In.Parameter_Problem (In.Option option3_recv,
				ip_recv_options (19, all_options))),
	 (Out.Parameter_Problem {pointer = SW.n8 "55",
				 data = ip_send_options (20, all_options)},
	  In.Parameter_Problem (In.End_Of_Options,
				ip_recv_options (20, all_options))),
	 (Out.Parameter_Problem {pointer = SW.n8 "56",
				 data = ip_send_options (21, all_options)},
	  In.Parameter_Problem (In.Data (SW.n8 "0"),
				ip_recv_options (21, all_options))),
	 (Out.Source_Quench (ip_send 22),
	  In.Source_Quench (ip_recv 22)),
	 (Out.Echo {id = SW.n16 "2", sequence = SW.n16 "3", data = ip_send 23},
	  In.Echo {id = SW.n16 "2", sequence = SW.n16 "3", data = ip_send 23}),
	 (Out.Echo_Reply {id = SW.n16 "1234", sequence = SW.n16 "5678",
			  data = ip_send 24},
	  In.Echo_Reply {id = SW.n16 "1234", sequence = SW.n16 "5678",
			 data = ip_send 24}),
	 (Out.Time_Stamp {id = SW.n16 "2345", sequence = SW.n16 "6789"},
	  In.Time_Stamp {id = SW.n16 "2345", sequence = SW.n16 "6789",
			 originate = SW.n32 "0x0", receive = SW.n32 "0"}),
	 (Out.Time_Stamp_Reply {id = SW.n16 "3456", sequence = SW.n16 "7890",
				originate = SW.n32 "0x02040608",
				receive = SW.n32 "0x01020304"},
	  In.Time_Stamp_Reply {id = SW.n16 "3456", sequence = SW.n16 "7890",
			       originate = SW.n32 "0x02040608",
			       receive = SW.n32 "0x01020304",
			       transmit = SW.n32 "0x05060708",
			       returned = SW.n32 "0x090a0b0c"}),
	 (Out.Mask_Request {id = SW.n16 "0x4567", sequence = SW.n16 "0x8901"},
	  In.Mask_Request {id = SW.n16 "0x4567", sequence = SW.n16 "0x8901"}),
	 (Out.Mask_Reply {id = SW.n16 "1", sequence = SW.n16 "2",
			  address_mask = SW.n32 "0xffff0000"},
	  In.Mask_Reply {id = SW.n16 "1", sequence = SW.n16 "2",
			 address_mask = SW.n32 "0xffff0000"})]

  end (* local *)


(*
	3.	function check_value
*)

  local
   fun check_arrays (name, a1, a2) =
        let val l1 = External.size a1
            val l2 = External.size a2
	    val d1 = External.sub (a1, {start = 0, length = l1})
	    val d2 = External.sub (a2, {start = 0, length = l2})
	    val same = Word_Array.W8.equal (d1, d2)
	in if same then ()
	   else
	    Trace.local_print (name ^ " is " ^ External.makestring a1 ^
			       " instead of " ^ External.makestring a2);
	   B.Test.test (name, fn _ => same)
	end

   fun same_problem (In.Header ptr1, In.Header ptr2) = ptr1 = ptr2
     | same_problem (In.Option opt1, In.Option opt2) =
        Ip_Header.Option.equal ([opt1], [opt2])
     | same_problem (In.End_Of_Options, In.End_Of_Options) = true
     | same_problem (In.Data ptr1, In.Data ptr2) = ptr1 = ptr2
     | same_problem _ = false

   fun check_same (name, eq, makestring, data1, data2) =
        if eq (data1, data2) then
	 B.Test.test (name, fn _ => true)
	else
	 (Trace.local_print (name ^ " is " ^ makestring data1 ^
			     " instead of " ^ makestring data2);
	  B.Test.test (name, fn _ => false))

   fun makestring_addr (SOME (src, dest, proto)) =
        "{source = " ^ FoxMakestring.word32 src ^ 
        ", destination = " ^ FoxMakestring.word32 dest ^ 
        ", protocol = " ^ FoxMakestring.word8 proto ^ "}"
     | makestring_addr NONE = ""

  in
   fun check_in (In.Unreachable (u1, d1), In.Unreachable (u2, d2)) =
        (check_same ("unreachable", op=, U.makestring, u1, u2);
	 check_arrays ("unreachable", d1, d2))
     | check_in (In.Transit_Time_Exceeded d1, In.Transit_Time_Exceeded d2) =
        check_arrays ("transit time exceeded", d1, d2)
     | check_in (In.Reassembly_Time_Exceeded d1,
		 In.Reassembly_Time_Exceeded d2) =
        check_arrays ("reassembly time exceeded", d1, d2)
     | check_in (In.Parameter_Problem (p1, d1),
		 In.Parameter_Problem (p2, d2)) =
        (check_same ("parameter problem header ", same_problem,
		     In.makestring_problem, p1, p2);
	 check_arrays ("parameter problem data", d1, d2))
     | check_in (In.Source_Quench d1, In.Source_Quench d2) =
        check_arrays ("source quench", d1, d2)
     | check_in (In.Echo_Reply {id = id1, sequence = seq1, data = d1},
		 In.Echo_Reply {id = id2, sequence = seq2, data = d2}) =
        (check_same ("echo reply: id", op=, FoxMakestring.word16, id1, id2);
         check_same ("echo reply: seq", op=, FoxMakestring.word16, seq1, seq2);
	 check_arrays ("echo reply: data", d1, d2))
     | check_in (In.Time_Stamp_Reply {id = i1, sequence = s1, originate = o1,
				      receive = r1, ...},
		 In.Time_Stamp_Reply {id = i2, sequence = s2, originate = o2,
				      receive = r2, ...}) =
(* do not check transmit or returned times, as they are not predictable *)
        (check_same ("time-stamp reply: id", op=,
		     FoxMakestring.word16, i1, i2);
         check_same ("time-stamp reply: seq", op=,
		     FoxMakestring.word16, s1, s2);
         check_same ("time-stamp reply: orig", op=,
		     FoxMakestring.word32, o1, o2);
         check_same ("time-stamp reply: recv", op=,
		     FoxMakestring.word32, r1, r2))
     | check_in (In.Redirect {reason = r1, new_gateway = g1, header = h1},
		 In.Redirect {reason = r2, new_gateway = g2, header = h2}) =
        (check_same ("redirect: reason", op=, R.makestring, r1, r2);
         check_same ("redirect: gateway", op=, FoxMakestring.word32, g1, g2);
	 check_arrays ("redirect: header", h1, h2))
     | check_in (In.Echo {id = id1, sequence = seq1, data = d1},
		 In.Echo {id = id2, sequence = seq2, data = d2}) =
        (check_same ("echo: id", op=, FoxMakestring.word16, id1, id2);
         check_same ("echo: seq", op=, FoxMakestring.word16, seq1, seq2);
	 check_arrays ("echo: data", d1, d2))
     | check_in (In.Time_Stamp {id = i1, sequence = s1,
				originate = o1, receive = r1},
		 In.Time_Stamp {id = i2, sequence = s2,
				originate = o2, receive = r2}) =
       (* do not check originate and receive times *)
        (check_same ("time-stamp: id", op=, FoxMakestring.word16, i1, i2);
         check_same ("time-stamp: seq", op=, FoxMakestring.word16, s1, s2))
     | check_in (In.Mask_Request {id = id1, sequence = seq1},
		 In.Mask_Request {id = id2, sequence = seq2}) =
        (check_same ("mask request: id", op=, FoxMakestring.word16, id1, id2);
         check_same ("mask request: seq", op=,
		     FoxMakestring.word16, seq1, seq2))
     | check_in (In.Mask_Reply {id = id1, sequence = seq1, address_mask = a1},
		 In.Mask_Reply {id = id2, sequence = seq2,
				address_mask = a2}) =
        (check_same ("mask reply: id", op=, FoxMakestring.word16, id1, id2);
         check_same ("mask reply: seq", op=, FoxMakestring.word16, seq1, seq2);
	 check_same ("mask reply: mask", op=, FoxMakestring.word32, a1, a2))
     | check_in (m1, m2) =
        (Trace.local_print ("message type " ^ In.makestring m1 ^
			    " <> " ^ In.makestring m2);
	 B.Test.test ("message type", fn _ => false))

  end (* local *)

(*
	4.	function run_tests
*)

  fun run_tests [] = ()
    | run_tests ((arg, expected_res) :: rest) =
       ((let val marshaled = Out.marshal arg
	     val unmarshaled = In.unmarshal marshaled
         in check_in (unmarshaled, expected_res);
	    run_tests rest
         end)
	  handle x =>
	          Trace.print_handled
		     (x, SOME ("run_tests, arg = " ^ Out.makestring arg ^
			       ", expected result = " ^
			       In.makestring expected_res)))

(*
	5.	function run
*)

  fun run () = B.Test.tests ("ICMP header", 58, fn _ => run_tests tests)

  val _ = if ! B.Debug.do_tests then run () else ()

 end (* struct *)

(*
	6.	structure Test_Icmp_Header
*)

structure Test_Icmp_Header = Test_Icmp_Header (structure B = Fox_Basis
					       val debug_level = NONE)






