(*

        FoxNet: The Fox Project's Communication Protocol Implementation Effort
        Edoardo Biagioni (Edoardo.Biagioni@cs.cmu.edu)
        Brian Milnes (Brian.Milnes@cs.cmu.edu)
        Ken Cline (Kenneth.Cline@cs.cmu.edu)
        Nick Haines (Nick.Haines@cs.cmu.edu)
        Fox Project
        School of Computer Science
        Carnegie Mellon University
        Pittsburgh, Pa 15139-3891

                i.      Abstract

        A test file for ICMP.


                ii.     Table of Contents

        i.      Abstract
        ii.     Table of Contents
        iii.    RCS Log
        1.      functor Test_Icmp
        2.      test_icmp_run
        3.      test_icmp_runs
        4.      test_icmp_passive
        5.      makestring_tests

                iii.    RCS Log
        
$Log: icmp.tst,v $
Revision 1.19  1995/03/24  16:02:53  esb
now works.

Revision 1.18  1995/03/12  17:55:39  esb
adapted to new trace.sig.

Revision 1.17  1995/03/07  23:43:54  esb
updated tracing.

Revision 1.16  1995/02/21  13:17:30  esb
upgraded for SML/NJ 1.07.

Revision 1.15  1995/02/13  23:29:38  esb
adapted to the new dynarray interface.

Revision 1.14  1995/01/18  21:07:24  esb
added scheduler resets and made possible to run multiple times.

Revision 1.13  1995/01/17  21:08:43  esb
adapted to new icmp.sig.

Revision 1.12  1995/01/16  23:46:49  esb
someone broke this severely by changing all the constants.  Fixed.

Revision 1.11  1995/01/06  17:04:51  esb
adapted to new Test_Addresses.

Revision 1.10  1994/11/22  13:56:55  milnes
Updated to initialize its own addressing so that the tests would run.

Revision 1.9  1994/11/07  21:37:03  cline
use V.String

Revision 1.8  1994/10/11  15:43:04  cline
Upgraded to work with SML/NJ 1.05b

Revision 1.7  1994/09/30  17:05:34  esb
changed DLOCS to Dyn_Locs.

Revision 1.6  1994/08/28  21:46:58  milnes
Completely changed.

Revision 1.5  1994/07/04  21:35:47  esb
adapted to Copy/Create split.

Revision 1.4  1994/07/01  02:36:09  danwang
Moved control structures into Fox_Basis.

Revision 1.3  1994/06/16  16:59:15  danwang
Updated for functorized Fox_Basis

Revision 1.2  1994/06/05  18:46:26  milnes
Extended.

Revision 1.1  1994/05/23  14:09:03  milnes
Initial revision

        1.      functor Test_Icmp
*)

functor Test_Icmp (structure B: FOX_BASIS
                   val debug_level: int ref option): TEST_STRUCTURE =
 struct
  structure Trace = Trace (structure V = B.V
			   val debug_level = debug_level
			   val module_name = "icmp.tst")
  val local_print = Trace.local_print
  val debug_print = Trace.debug_print
  val debug_constant_string = Trace.debug_constant_string
  val no_debug_level = NONE

  structure Sim = Build_Simulators (structure B = B
                                    val debug_level = no_debug_level)

  val ip_over_eth = SW.n16 "0x888"
  val quick = Test_Addresses.get_ip "quick"
  val snow = Test_Addresses.get_ip "snow"
  val loop = SW.n32 "0x7f000001"

  local 
   structure B1 = Build_Icmp (structure Device = Sim.Snow
                              structure B = B
			      val ip_over_eth = ip_over_eth
	                      val serve_echos_and_address_masks = false
                              val log_icmp_echos = true
			      val eth_debug_level = no_debug_level
			      val arp_debug_level = no_debug_level
			      val ip_debug_level = debug_level
			      val icmp_debug_level = debug_level)
   in 
    fun init_snow () =
         (B1.Ip_Mux.add ("SE0", Test_Addresses.get_ip "snow");
	  B1.Ip.initialize ();
	  B1.Ip.set_interface_address ("SE0", Test_Addresses.get_ip "snow"))
    fun fin_snow () = 
	 (B1.Ip.finalize ();
	  ())
    structure Snow_Icmp = B1.Icmp
    structure Snow_Ip = B1.Ip
   end

   local 
    structure B2 = Build_Icmp (structure Device = Sim.Quick
                               structure B = B
			       val ip_over_eth = ip_over_eth
                               val serve_echos_and_address_masks = false
                               val log_icmp_echos = true
			       val eth_debug_level = no_debug_level
			       val arp_debug_level = no_debug_level
			       val ip_debug_level = no_debug_level
			       val icmp_debug_level = debug_level)

   in
    fun init_quick () =
         (B2.Ip_Mux.add ("SE0", Test_Addresses.get_ip "quick");
	  B2.Ip.initialize ();
	  B2.Ip.set_interface_address ("SE0", Test_Addresses.get_ip "quick"))
    fun fin_quick () = 
	 (B2.Ip.finalize ();
	  ())
    structure Quick_Icmp = B2.Icmp
    structure Quick_Ip = B2.Ip
   end

  val init_stacks = init_quick o init_snow
  val fin_stacks = fin_quick o fin_snow

  val quick_address_for_snow = Quick_Icmp.Icmp_Address snow
  val snow_address_for_quick = Snow_Icmp.Icmp_Address quick

(*

        2.      test_icmp_run

*)

  fun compare (expected, p) () =
       let fun loop ([], []) = true
	     | loop (_, []) = false
	     | loop ([], _) = false
	     | loop (#"1" :: #"u" :: s, t) = loop (s, t)
	     | loop (s, #"1" :: #"u" :: t) = loop (s, t)
	     | loop (#"2" :: #"u" :: s, t) = loop (s, t)
	     | loop (s, #"2" :: #"u":: t) = loop (s, t)
	     | loop (#"4" :: #"u":: s, t) = loop (s, t)
	     | loop (s, #"4" :: #"u" :: t) = loop (s, t)
	     | loop (s0 :: s, t0 :: t) = s0 = t0 andalso loop (s,t)
	   val explode = B.V.String.explode
	   fun compare_strings (s0, s1) = loop (explode s0, explode s1)
       in if compare_strings (Quick_Icmp.makestring_incoming (p, NONE),
			      expected) then true
	  else
	   (local_print ("icmp.tst: error in test_icmp_run: received \"" ^
			 Quick_Icmp.makestring_incoming (p, NONE) ^
			 "\", expected \"" ^ expected ^ "\"\n");
			 false)
       end

  fun test_icmp_run (name, conn_to_sender, expected) =
       let val pipe = B.Pipe.new NONE
	   fun test_receive packet =
	        (B.Test.test (name, compare (expected, packet));
		 B.Pipe.enqueue (pipe, ()))
	   fun test_handler connection = (test_receive, fn _ => ())
	   fun ignore_handler _ = (fn _ => (), fn _ => ())
	   val _ = (debug_constant_string "About to initialize quick.";
		    Quick_Icmp.initialize ();
		    debug_constant_string "About to initialize snow.";
		    Snow_Icmp.initialize ();
		    debug_constant_string "Done initializing snow.")
	   val _ = debug_constant_string "Quick trying to connect to snow."
	   val quick_connection_to_snow = 
	        Quick_Icmp.connect (quick_address_for_snow,
				    Quick_Icmp.Handler test_handler)
	   val _ = debug_constant_string "Snow trying to connect to quick."
	   val snow_connection_to_quick = 
	        Snow_Icmp.connect (snow_address_for_quick,
				   Snow_Icmp.Handler ignore_handler)
	   val snow_send = (conn_to_sender snow_connection_to_quick)
	   val sleep_time = 1000
	   fun sleeper () =
	        (B.Scheduler.sleep sleep_time;
		 B.Pipe.enqueue (pipe, ()))
       in Quick_Icmp.set_service (quick_connection_to_snow, Quick_Icmp.Off);
	  B.Scheduler.fork sleeper;
	  debug_constant_string "in test_icmp_run; snow_send";
	  snow_send ();
	  debug_constant_string "sleep ";
	  B.Pipe.dequeue pipe;
	  debug_constant_string "close quick_connection_to_snow ";
	  Quick_Icmp.close quick_connection_to_snow;
	  debug_constant_string "close snow_connection_to_quick";
	  Snow_Icmp.close snow_connection_to_quick;
	  debug_constant_string "Quick_Icmp.finalize ()";
	  Quick_Icmp.finalize ();
	  debug_constant_string "Snow_Icmp.finalize ()";
	  Snow_Icmp.finalize ();
	  ()
       end (* let *)

  fun test_makestring_address () =
       if Snow_Icmp.makestring_address (Snow_Icmp.Icmp_Address (SW.n32 "0")) =
          "0.0.0.0" then true
       else
	(local_print ("got " ^
		      Snow_Icmp.makestring_address (Snow_Icmp.Icmp_Address
						    (SW.n32 "0")) ^
		      ", wanted '" ^ "0.0.0.0" ^ "'");
	 false)

  fun test_makestring_connection () =  true

  val test_array = ByteArray.array(10,255)

(*
        3.      test_icmp_runs
*)

  structure S = Snow_Icmp
  structure SO = Snow_Icmp.Out

  fun illegal_allocation (routine, packet, sender) = 
       (debug_print (fn _ => "a " ^ routine ^ " allocate_send call returned " ^
		     Snow_Icmp.makestring_outgoing (packet, NONE));
	sender)

  fun fill (d, l) = 
       let fun n (i, _) = FoxWord8.intToWord i
       in B.Dyn_Array.app1 (d, n)
       end

  fun test_unreachable conn =
       let val data = B.Dyn_Array.init1 (64, FoxWord8.intToWord)
	   val allocation = SO.Unreachable (S.Protocol_Unreachable, data)
	   val (_, sender) = S.allocate_send (conn, allocation)
       in sender
       end

  fun test_time_exceeded conn =
       let val data = B.Dyn_Array.init1 (64, FoxWord8.intToWord)
	   val allocation = SO.Transit_Time_Exceeded data
	   val (_, sender) = S.allocate_send (conn, allocation)
       in sender
       end

  fun test_parameter_problem conn =
       let val data = B.Dyn_Array.init1 (21, FoxWord8.intToWord)
	   val _ = B.Dyn_Array.update1 (data, 0, SW.n8 "0x45")
	   val allocation = SO.Parameter_Problem {pointer = SW.n8 "0xfd",
						  data = data}
	   val (_, sender) = S.allocate_send (conn, allocation)
       in sender
       end

  fun test_source_quench conn =
       let val data = B.Dyn_Array.init1 (64, FoxWord8.intToWord)
	   val allocation = SO.Source_Quench data
	   val (_, sender) = S.allocate_send (conn, allocation)
       in sender
       end

  fun test_echo conn =
       let val data = B.Dyn_Array.init1 (64, FoxWord8.intToWord)
	   val allocation = SO.Echo {id = SW.n16 "0", sequence = SW.n16 "0x1",
				     data = data}
	   val (_, sender) = S.allocate_send (conn, allocation)
       in sender
       end

  fun test_time_stamp conn =
       let val data = B.Dyn_Array.init1 (64, FoxWord8.intToWord)
	   val allocation = SO.Time_Stamp {id = SW.n16 "0",
					   sequence = SW.n16 "256",
					   originate = SW.n32 "2"}
	   val (_, sender) = S.allocate_send (conn, allocation)
       in sender
       end

  fun test_address_mask conn =
       let val allocation = SO.Mask_Request {id = SW.n16 "0",
					     sequence = SW.n16 "1"}
	   val (_, sender) = S.allocate_send (conn, allocation)
       in sender
       end

  fun test_icmp_runs () =
       (test_icmp_run ("destination unreachable", test_unreachable,
		       "destination unreachable: protocol unreachable");
	test_icmp_run ("transit time exceeded", test_time_exceeded,
		       "transit time exceeded");
	test_icmp_run ("parameter problem", test_parameter_problem,
		       "parameter problem: data problem at position 233, " ^
		       "data = 0x14");
	test_icmp_run ("source quench", test_source_quench, "source quench");
	test_icmp_run ("echo", test_echo,
		       "echo (id = 0, sequence = 1, data = " ^
		       "0x0.0x1.0x2.0x3.0x4.0x5.0x6.0x7." ^
		       "0x8.0x9.0xA.0xB.0xC.0xD.0xE.0xF." ^
		       "0x10.0x11.0x12.0x13.0x14.0x15.0x16.0x17." ^
		       "0x18.0x19.0x1A.0x1B.0x1C.0x1D.0x1E.0x1F." ^
		       "0x20.0x21.0x22.0x23.0x24.0x25.0x26.0x27." ^
		       "0x28.0x29.0x2A.0x2B.0x2C.0x2D.0x2E.0x2F." ^
		       "0x30.0x31.0x32.0x33.0x34.0x35.0x36.0x37." ^
		       "0x38.0x39.0x3A.0x3B.0x3C.0x3D.0x3E.0x3F)");
	test_icmp_run ("time stamp", test_time_stamp,
		       "time stamp (id = 0, sequence = 256, originate = 2)");
	test_icmp_run ("address mask", test_address_mask,
		       "address mask request (id = 0, sequence = 1)"))

(*
        4.      test_icmp_passive
*)

  fun test_icmp_passive (snow_address_for_quick, quick_address_for_snow) = 
       let fun compare (expected, p) () =
                if Quick_Icmp.makestring_incoming (p, NONE) = expected then
	         true
	        else
	         (local_print ("error in test_icmp_passive: received \"" ^
			       Quick_Icmp.makestring_incoming (p, NONE) ^
			       "\", expected \"" ^ expected ^ "\"");
		  false)
           val expected = "destination unreachable: protocol unreachable"
	   val pipe = B.Pipe.new NONE
	   fun test_receive packet =
	        (B.Test.test ("icmp passive", compare (expected, packet));
		 B.Pipe.enqueue (pipe, ()))
	   fun test_handler connection = (test_receive, fn _ => ())
           fun ignore_handler _ = (fn _ => (), fn _ => ())
           val _ = (debug_constant_string "About to initialize quick.";
		    Quick_Icmp.initialize ();
		    debug_constant_string "About to initialize snow.";
		    Snow_Icmp.initialize ();
		    debug_constant_string "Done initializing snow.")
           val _ = debug_constant_string "Quick starting passive"
           val quick_connection_to_snow =
	        Quick_Icmp.start_passive (quick_address_for_snow,
				          Quick_Icmp.Handler test_handler,
					  SOME 1)
           val _ = debug_constant_string "Snow trying to connect to quick"
           val snow_connection_to_quick =
                Snow_Icmp.connect (snow_address_for_quick,
			           Snow_Icmp.Handler ignore_handler)
           val sleep_time = 1000
	   fun sleeper () =
	        (B.Scheduler.sleep sleep_time;
		 B.Pipe.enqueue (pipe, ()))
	   val _ = debug_constant_string "in test_icmp_passive"
	   val _ = B.Scheduler.fork sleeper;
	   val data = B.Dyn_Array.init1 (64, FoxWord8.intToWord)
	   val (_, sender) =
	        S.allocate_send (snow_connection_to_quick,
				 SO.Unreachable (S.Protocol_Unreachable, data))
       in debug_constant_string "calling the sender"; 
	  sender ();
          B.Pipe.dequeue pipe;
          Snow_Icmp.close snow_connection_to_quick;
          Quick_Icmp.finalize ();
          Snow_Icmp.finalize ();
          ()
       end (* let *)

(*
        5.      makestring_tests
*)

  fun run_makestring_tests () =
       (B.Test.test ("makestring address", test_makestring_address);
        B.Test.test ("makestring connection",test_makestring_connection))

  fun run_tests () =
       (test_icmp_runs ();
   (* This test probably won't work if do_prints is true. *)
        test_icmp_passive 
             (Snow_Icmp.Icmp_Address (Test_Addresses.get_ip "quick"),
              Quick_Icmp.Icmp_Address (Test_Addresses.get_ip "snow"));
        test_icmp_passive 
             (Snow_Icmp.Icmp_Address (Test_Addresses.get_ip "quick"),
              Quick_Icmp.Icmp_Address (Test_Addresses.get_ip "snow"));
        run_makestring_tests ();
        ())

  fun run () =
       (B.Scheduler.reset ();
	init_stacks ();
	B.Test.tests ("Icmp", 11, run_tests);
	fin_stacks ())

  val _ = if ! B.Debug.do_tests then run () else ()

 end (* struct *)

structure Test_Icmp = Test_Icmp (structure B = Fox_Basis
                                 val debug_level = NONE)
