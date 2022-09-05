(*

	FoxNet: The Fox Project's Communication Protocol Implementation Effort
	Edoardo Biagioni (esb@cs.cmu.edu)
	Brian Milnes (Brian.Milnes@cs.cmu.edu)
	Nick Haines  (Nick.Haines@cs.cmu.edu)
	Fox Project
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, Pa 15139-3891

		i.	Abstract

	Test code to test the ethernet device simulator.


		ii.	Table of Contents

	i.	Abstract
	ii.	Table of Contents
	iii.	RCS Log
	1.	functor Test_Simulator
	2.	structure Test_Simulator

		iii.	RCS Log
	
$Log: sim.tst,v $
Revision 1.22  1995/03/12  17:53:50  esb
adapted to new trace.sig.

Revision 1.21  1995/03/10  03:48:16  esb
adapted to new vendor.sig.

Revision 1.20  1995/03/07  20:37:42  esb
updated tracing.

Revision 1.19  1995/02/09  19:50:47  esb
made work with sml/nj 1.07

Revision 1.18  1995/01/06  17:03:20  esb
adapted to new Test_Addresses.

Revision 1.17  1994/10/11  15:43:04  cline
Upgraded to work with SML/NJ 1.05b

Revision 1.16  1994/08/24  22:24:19  esb
made to work with incoming packet size not same as outgoing packet size.

Revision 1.15  1994/08/02  20:25:36  esb
adapted to new protocol signature.

Revision 1.14  1994/07/01  02:34:39  danwang
Moved control structures into Fox_Basis.

Revision 1.13  1994/06/16  16:45:05  danwang
Updated to use functorized Fox_Basis

Revision 1.12  1994/05/23  14:04:32  milnes
Added print functions.

Revision 1.11  1994/04/06  23:21:50  esb
adapted to new receive_packet interface.

Revision 1.10  94/03/02  21:21:25  esb
removed unused functors, improved the test.

Revision 1.9  94/01/18  15:14:17  esb
restructured.

Revision 1.8  1993/12/04  20:56:27  esb
now provide a handler with a parameter of type connection.

Revision 1.7  1993/10/25  19:35:56  cline
removed .U from Byte[421].U

Revision 1.6  1993/09/02  15:54:16  esb
added the Fox Basis B.

Revision 1.5  1993/07/12  20:38:17  esb
changed to work with the new dev.sig using Send_Packet and Receive_Packet

Revision 1.4  1993/06/19  01:49:22  esb
shortened the name of the test

Revision 1.3  1993/06/18  15:48:38  esb
added the Coroutine structure to the functor parameters

Revision 1.2  1993/06/15  23:21:33  esb
integrated new passive_open in which the handler may fail

Revision 1.1  1993/06/10  23:05:01  milnes
Initial revision


	1.	functor Test_Simulator
*)
 
functor Test_Simulator (structure B: FOX_BASIS
			val debug_level: int ref option) : TEST_STRUCTURE =
 struct
  structure Trace = Trace (structure V = B.V
			   val debug_level = debug_level
			   val module_name = "sim.tst")
  val local_print = Trace.local_print
  val do_prints = Trace.debug_on ()

  structure Sim = Build_Simulators (structure B = B
				    val debug_level = debug_level)

  structure Snow = Sim.Snow
  structure Quick = Sim.Quick

  val snow = Test_Addresses.name_eth "snow"
  val quick = Test_Addresses.name_eth "quick"
  val broadcast = {a0 = SW.n8 "0xff", a1 = SW.n8 "0xff", a2 = SW.n8 "0xff",
		   a3 = SW.n8 "0xff", a4 = SW.n8 "0xff", a5 = SW.n8 "0xff"}

  fun fill_in_at (b, n, {a0, a1, a2, a3, a4, a5}) =
       (B.Dyn_Array.update1 (b, n,     a0);
	B.Dyn_Array.update1 (b, n + 1, a1);
	B.Dyn_Array.update1 (b, n + 2, a2);
	B.Dyn_Array.update1 (b, n + 3, a3);
	B.Dyn_Array.update1 (b, n + 4, a4);
	B.Dyn_Array.update1 (b, n + 5, a5))

  fun fill_in_from (b, a) = fill_in_at (b, 6, a)

  fun fill_in_to (b, a) = fill_in_at (b, 0, a)

  fun fill_data_one_to_10 (b, off) =
       (B.Dyn_Array.update1 (b, 12 + off, SW.n8 "0");
	B.Dyn_Array.update1 (b, 13 + off, SW.n8 "0");
	B.Dyn_Array.update1 (b, 14 + off, SW.n8 "0");
	B.Dyn_Array.update1 (b, 15 + off, SW.n8 "0");
	B.Dyn_Array.update1 (b, 16 + off, SW.n8 "1");
	B.Dyn_Array.update1 (b, 17 + off, SW.n8 "2");
	B.Dyn_Array.update1 (b, 18 + off, SW.n8 "3");
	B.Dyn_Array.update1 (b, 19 + off, SW.n8 "4");
	B.Dyn_Array.update1 (b, 20 + off, SW.n8 "5");
	B.Dyn_Array.update1 (b, 21 + off, SW.n8 "6");
	B.Dyn_Array.update1 (b, 22 + off, SW.n8 "7");
	B.Dyn_Array.update1 (b, 23 + off, SW.n8 "8");
	B.Dyn_Array.update1 (b, 24 + off, SW.n8 "9");
	B.Dyn_Array.update1 (b, 25 + off, SW.n8 "10"))

  fun init_send (packet, src, dest) =
       (fill_in_from (packet, src);
	fill_in_to (packet, dest);
	fill_data_one_to_10 (packet, 0);
	packet)

  fun init_receive (src, dest) =
       let val packet = B.Dyn_Array.new 28
       in fill_in_from (packet, src);
	  fill_in_to (packet, dest);
	  fill_data_one_to_10 (packet, 2);
	  packet
       end

  val quick_send = init_send (B.Dyn_Array.new 26, quick, snow)
  val quick_receive = init_receive (snow, broadcast)
  val snow_send = init_send (B.Dyn_Array.new 26, snow, broadcast)
  val snow_receive = init_receive (quick, snow)

  val send_read = B.Dyn_Array.read

  fun receive_read packet = B.Dyn_Array.sub (packet, 0, 28)

  val result = ref ([]: (string * ByteArray.bytearray) list)
  val quick_result = [("sent from quick", send_read quick_send),
		      ("received on snow", receive_read snow_receive)]
  val snow_result = [("received on snow", receive_read quick_receive),
		     ("received on quick", receive_read quick_receive),
		     ("sent from snow", send_read snow_send)]

  fun receive_and_print hostname bytes =
       (if do_prints then
	 local_print ("sim.tst: received on " ^ hostname ^ ": " ^
		      B.Access.to_string (receive_read bytes))
	else ();
	result := (! result) @ [("received on " ^ hostname,
				 receive_read bytes)])

  fun sent_from_print (hostname, bytes) =
       (if do_prints then
	 local_print ("sim.tst: sent from " ^ hostname ^ ": " ^
		      B.Access.to_string (receive_read bytes))
	else ();
	result := (! result) @ [("sent from " ^ hostname,
				 send_read bytes)])

  val to_all =
       let val packet = B.Dyn_Array.new 26
       in fill_in_to (packet, broadcast);
	  fill_in_from (packet, snow);
	  fill_data_one_to_10 (packet, 0);
	  packet
       end

  val quick_receive_fun = receive_and_print "quick"
  val snow_receive_fun = receive_and_print "snow"
  fun ignore_fun _ =
       local_print "function called that should not have been called"

  val quick_handler = Quick.Handler (fn _ => (quick_receive_fun, ignore_fun))
  val snow_handler = Snow.Handler (fn _ => (snow_receive_fun, ignore_fun))

  fun run_sim_tests () = 
       let val _ = Snow.initialize ()
	   val _ = Quick.initialize ()
	   val (quick_stop, quick_passives) =
	            Quick.start_passive ((), quick_handler, SOME 1)
	   val snow_conn = Snow.connect ((), snow_handler)
	   fun array_same (a, b, n) =
	        if n >= ByteArray.length a andalso n >= ByteArray.length b then
		 true
		else if n >= ByteArray.length a orelse
	                n >= ByteArray.length b then
	         (print ("lengths are " ^ (makestring (ByteArray.length a)) ^
			 " and " ^ (makestring (ByteArray.length b)));
		  false)
		else if FoxWord8.sub (a, n) <> FoxWord8.sub (b, n) then
		 (print ("difference at position " ^ (makestring n));
		  false)
	        else
		 array_same (a, b, n + 1)
	   fun result_same ([], []) = true
	     | result_same ([], _) =
	        (local_print "actual result is shorter";
		 false)
	     | result_same (_, []) =
	        (local_print "actual result is longer";
		 false)
	     | result_same ((sa, da) :: ra, (sb, db) :: rb) =
		sa = sb andalso array_same (da, db, 0) andalso
		result_same (ra, rb)
	   fun same_set (s1, s2, same_element) = 
	        let fun member (a, []) = false
		      | member (a, head :: rest) =
			if same_element (a, head) then true
			else member (a, rest)
		    fun subset (set, []) = true
		      | subset (set, head :: rest) =
			member (head, set) andalso subset (set, rest)
		in subset (s1, s2) andalso subset (s2, s1)
		end
	   fun array_makestring (data, n) =
	        if n >= ByteArray.length data then ""
		else ((FoxMakestring.word8 (FoxWord8.sub (data, n))) ^ "." ^
		      (array_makestring (data, n + 1)))
	   fun res_makestring [] = ""
	     | res_makestring ((s, data) :: rest) =
		(s ^ ": " ^ (array_makestring (data, 0)) ^
		 (res_makestring rest))
	   fun check_result res =
	        if result_same (! result, res) then true
		else (local_print ("result is " ^ (res_makestring (! result)) ^
				   "\nexpecting " ^ (res_makestring res));
		      false)
	   fun quick_connection () =
	        case quick_passives () of
		   [] => (local_print "error: no passive connections";
			  ())
		 | [c] => c
		 | (head :: rest) =>
		    (local_print "error: more than one passive connection";
		     head)
	   fun quick_send_fun () =
	        (result := [];
		 sent_from_print ("quick", quick_send);
		 let val conn = quick_connection ()
		     val size = 26
		     val (packet, send) = Quick.allocate_send (conn, size)
		 in init_send (packet, quick, snow);
		    send ()
		 end;
		 check_result quick_result)
	   fun snow_send_fun () =
	        (result := [];
		 let val size = 26
		     val (packet, send) = Snow.allocate_send (snow_conn, size)
		 in init_send (packet, snow, broadcast);
		    send ()
		 end;
		 sent_from_print ("snow", to_all);
		 check_result snow_result)
       in B.Test.test ("quick to snow", quick_send_fun);
	  sent_from_print ("snow", snow_send);
	  B.Test.test ("snow to quick", snow_send_fun);
	  Snow.close snow_conn;
	  Quick.close (quick_connection ());
	  Snow.finalize ();
	  Quick.finalize ();
	  ()
       end (* let *)

  fun test_makestring_address () = Quick.makestring_address () = "()"

  fun test_makestring_incoming () = 
       let fun init i = FoxWord8.intToWord (i mod 256)
	   val packet = B.Dyn_Array.init1 (50, init)
	   val dest1 = "\nTo    = 1ux00.1ux01.1ux02.1ux03.1ux04.1ux05"
	   val source1 = "\nFrom  = 1ux06.1ux07.1ux08.1ux09.1ux0a.1ux0b"
	   val proto1 = "\nproto = 1ux0c.1ux0d"
	   val data10 = "\ndata  = 1ux0e.1ux0f.1ux10.1ux11.1ux12.1ux13.1ux14."
	   val data11 = "1ux15.1ux16.1ux17.1ux18.1ux19.1ux1a.1ux1b.1ux1c."
	   val data12 = "1ux1d.1ux1e.1ux1f.1ux20.1ux21.1ux22.1ux23.1ux24."
	   val data13 = "1ux25.1ux26.1ux27.1ux28.1ux29.1ux2a.1ux2b.1ux2c."
	   val data14 = "1ux2d.1ux2e.1ux2f.1ux30.1ux31\n"
	   val output1 = dest1 ^ source1 ^ proto1 ^
                         data10 ^ data11 ^ data12 ^ data13 ^ data14
	   val dest2 = "\nTo    = 1ux00.1ux01.1ux02.1ux03.1ux04.1ux05"
	   val source2 = "\nFrom  = 1ux06.1ux07.1ux08.1ux09.1ux0a.1ux0b"
	   val proto2 = "\nproto = 1ux0c.1ux0d"
	   val data20 = "\ndata  = 1ux0e.1ux0f.1ux10.1ux11.1ux12.1ux13.1ux14."
	   val data21 = "1ux15.1ux16.1ux17.1ux18.1ux19.1ux1a.1ux1b.1ux1c."
	   val data22 = "1ux1d.1ux1e.1ux1f.1ux20.1ux21...\n"
	   val output2 = dest2 ^ source2 ^ proto2 ^ data20 ^ data21 ^ data22
       in Quick.makestring_incoming (packet, NONE) = output1 andalso
	  Quick.makestring_incoming (packet, SOME 20) = output2
       end

  fun test_makestring_outgoing () = 
       let fun init i = FoxWord8.intToWord (i mod 256)
	   val packet = B.Dyn_Array.init1 (50, init)
	   val dest1 = "\nTo    = 1ux00.1ux01.1ux02.1ux03.1ux04.1ux05"
	   val source1 = "\nFrom  = 1ux06.1ux07.1ux08.1ux09.1ux0a.1ux0b"
	   val proto1 = "\nproto = 1ux0c.1ux0d"
	   val data10 = "\ndata  = 1ux0e.1ux0f.1ux10.1ux11.1ux12.1ux13.1ux14."
	   val data11 = "1ux15.1ux16.1ux17.1ux18.1ux19.1ux1a.1ux1b.1ux1c."
	   val data12 = "1ux1d.1ux1e.1ux1f.1ux20.1ux21.1ux22.1ux23.1ux24."
	   val data13 = "1ux25.1ux26.1ux27.1ux28.1ux29.1ux2a.1ux2b.1ux2c."
	   val data14 = "1ux2d.1ux2e.1ux2f.1ux30.1ux31\n"
	   val output1 = dest1 ^ source1 ^ proto1 ^
                         data10 ^ data11 ^ data12 ^ data13 ^ data14
	   val dest2 = "\nTo    = 1ux00.1ux01.1ux02.1ux03.1ux04.1ux05"
	   val source2 = "\nFrom  = 1ux06.1ux07.1ux08.1ux09.1ux0a.1ux0b"
	   val proto2 = "\nproto = 1ux0c.1ux0d"
	   val data20 = "\ndata  = 1ux0e.1ux0f.1ux10.1ux11.1ux12.1ux13.1ux14."
	   val data21 = "1ux15.1ux16.1ux17.1ux18.1ux19.1ux1a.1ux1b.1ux1c."
	   val data22 = "1ux1d.1ux1e.1ux1f.1ux20.1ux21...\n"
	   val output2 = dest2 ^ source2 ^ proto2 ^ data20 ^ data21 ^ data22
       in if Quick.makestring_outgoing (packet, NONE) = output1 andalso
	     Quick.makestring_outgoing (packet, SOME 20) = output2 then true
          else if Quick.makestring_outgoing (packet, NONE) <> output1 then
	   (local_print ("makestring_outgoing (1), expecting " ^ output1 ^
			 "\ngot " ^
			 (Quick.makestring_outgoing (packet, NONE)));
	    false)
          else
	   (local_print ("makestring_outgoing (2), expecting " ^ output2 ^
			 "\ngot " ^
			 (Quick.makestring_outgoing (packet, SOME 20)));
	    false)
       end

  fun run_makestring_tests () =
       (B.Test.test ("makestring_address", test_makestring_address);
	B.Test.test ("makestring_incoming", test_makestring_incoming);
	B.Test.test ("makestring_outgoing", test_makestring_outgoing))

  fun run_tests () =
       (B.Scheduler.reset ();
	run_sim_tests ();
	run_makestring_tests ())

  fun run () = 
       if B.Debug.include_tests then B.Test.tests ("Sim", 5, run_tests)
       else ()

  val _ = if ! B.Debug.do_tests then run () else ()

 end (* struct *)

(*
	2.	structure Test_Simulator
*)

structure Test_Simulator = Test_Simulator (structure B = Fox_Basis
					   val debug_level = NONE)
