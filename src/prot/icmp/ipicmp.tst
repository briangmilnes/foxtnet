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
        1.      functor Test_Ip_Icmp
        2.      test_ipicmp
        3.      test_ipicmp_passive
        5.      makestring_tests

                iii.    RCS Log
        
$Log: ipicmp.tst,v $
Revision 1.14  1995/03/24  16:02:53  esb
now works.

Revision 1.13  1995/03/12  17:55:39  esb
adapted to new trace.sig.

Revision 1.12  1995/03/07  23:43:54  esb
updated tracing.

Revision 1.11  1995/02/21  13:17:30  esb
upgraded for SML/NJ 1.07.

Revision 1.10  1995/02/13  23:29:38  esb
adapted to the new dynarray interface.

Revision 1.9  1995/01/18  21:07:24  esb
added scheduler resets and made possible to run multiple times.

Revision 1.8  1995/01/17  21:08:43  esb
adapted to new icmp.sig.

Revision 1.7  1995/01/06  17:04:51  esb
adapted to new Test_Addresses.

Revision 1.6  1994/11/22  13:56:55  milnes
Updated to initialize its own addressing so that the tests would run.

Revision 1.5  1994/11/07  21:37:20  cline
use V.String

Revision 1.4  1994/10/11  15:43:04  cline
Upgraded to work with SML/NJ 1.05b

Revision 1.3  1994/09/30  17:05:34  esb
changed DLOCS to Dyn_Locs.

Revision 1.2  1994/08/28  21:47:28  milnes
Added default gateways.

Revision 1.1  1994/08/25  23:44:00  milnes
Initial revision


        1.      functor Test_Ip_Icmp
*)

functor Test_Ip_Icmp (structure B: FOX_BASIS
                      val debug_level: int ref option): TEST_STRUCTURE  =
 struct
  structure Trace = Trace (structure V = B.V
			   val debug_level = debug_level
			   val module_name = "ipicmp.tst")
  val local_print = Trace.local_print
  val debug_print = Trace.debug_print
  val no_debug_level = NONE

  structure Sim = Build_Simulators (structure B = B
                                    val debug_level = no_debug_level)

  val ip_over_eth = SW.n16 "0x888"
  val quick = Test_Addresses.get_ip "quick"
  val snow  = Test_Addresses.get_ip "snow"
  val loop = SW.n32 "0x7f000001"

  local 
   structure B1 = 
      Build_Ip_Icmp (structure Device = Sim.Snow
		     structure B = B
		     val ip_over_eth = ip_over_eth
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
   structure B2 = Build_Ip_Icmp (structure Device = Sim.Quick
				 structure B = B
				 val ip_over_eth = ip_over_eth
				 val log_icmp_echos = true
				 val eth_debug_level = no_debug_level
				 val arp_debug_level = no_debug_level
				 val ip_debug_level = debug_level
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

  val proto = SW.n8 "0"
  val quick_ip_address_for_snow  =  Quick_Ip.Address {ip = snow, proto = proto}
  val snow_ip_address_for_quick  =  Snow_Ip.Address {ip = quick, proto = proto}
  val snow_icmp_address_for_quick = Snow_Icmp.Icmp_Address quick

(*
	2.	test_ipicmp
*)

  datatype data_or_status = Data of (Snow_Ip.connection -> unit -> unit)
                          | Status of (Snow_Icmp.connection -> unit -> unit)

  fun test_ipicmp (name, datastatus, expected) =
       let fun compare_strings (s0,s1) =
	        let fun loop ([],[]) = true
		      | loop (_,[]) = false
		      | loop ([],_) = false
		      | loop (#"1" :: #"u" :: s, t) = loop (s,t)
		      | loop (s, #"1" :: #"u" :: t) = loop (s,t)
		      | loop (#"2" :: #"u" :: s, t) = loop (s,t)
		      | loop (s, #"2" :: #"u" :: t) = loop (s,t)
		      | loop (#"4" :: #"u" :: s, t) = loop (s,t)
		      | loop (s, #"4" :: #"u" :: t) = loop (s,t)
		      | loop (s0 :: s, t0 :: t) = s0 = t0 andalso loop (s,t)
		    val explode = B.V.String.explode
		in loop (explode s0, explode s1)
		end

	   fun compare_data (expected_data, p) () = 
	        (if compare_strings (Quick_Ip.makestring_incoming (p, NONE),
				     expected) then
		  true
		 else
		  (local_print ("error in test_icmp_run: received \"" ^
				Quick_Ip.makestring_incoming (p, NONE) ^
				"\", expected \"" ^ expected ^ "\"");
		   case datastatus of
		      Data _ => ()
		    | Status _ =>
		       debug_print (fn _ => "expected data, got status.");
		   false))
	   fun compare_status (expected_status, p) () = 
	        (debug_print (fn _ => "compare_status got status packet");
		 if compare_strings (Quick_Ip.makestring_status p,
				     expected) then
		  true
		 else
		  (local_print ("error in test_icmp_run: received \"" ^
				Quick_Ip.makestring_status p ^
				"\", expected \"" ^ expected ^ "\"");
                   case datastatus of
	              Data _ =>
		       debug_print (fn _ => "expected data, got status.")
		    | Status _ => ();
                   false))
	   fun test_receive connection =
	        (fn p => B.Test.test (name, compare_data (expected, p)),
		 fn p => B.Test.test (name, compare_status (expected, p)))
	   fun ignore_handler _ = (fn _ => (), fn _ => ())
	   val _ = (debug_print (fn _ => "About to initialize quick");
		    Quick_Ip.initialize ();
		    debug_print (fn _ => "About to initialize snow");
		    Snow_Ip.initialize ();
		    debug_print (fn _ => "Done initializing snow"))
	   val _ = debug_print (fn _ => "Quick trying to ip connect to snow")
	   val quick_ip_connection_to_snow =
	        Quick_Ip.connect (quick_ip_address_for_snow,
				  Quick_Ip.Handler test_receive)
	   val _ = debug_print (fn _ => "Snow trying to icmp connect to quick")
	   val snow_icmp_connection_to_quick = 
	        Snow_Icmp.connect (snow_icmp_address_for_quick,
				   Snow_Icmp.Handler ignore_handler)
	   val _ = debug_print (fn _ => "Snow trying to ip connect to quick")
	   val snow_ip_connection_to_quick = 
	        Snow_Ip.connect (snow_ip_address_for_quick,
				 Snow_Ip.Handler ignore_handler)
	   val sleep_time = 1000
       in debug_print (fn _ => "in test_ipicmp");
          case datastatus of
             Data f => (f snow_ip_connection_to_quick) ()
           | Status f => (f snow_icmp_connection_to_quick)();
          B.Scheduler.sleep sleep_time;
	  Quick_Ip.close quick_ip_connection_to_snow;
	  Snow_Ip.close snow_ip_connection_to_quick;
	  Snow_Icmp.close snow_icmp_connection_to_quick;
	  Quick_Ip.finalize ();
	  Snow_Ip.finalize ();
	  ()
       end (* let *)

  fun fill (d, l) = 
       let fun n (i, _) = FoxWord8.intToWord i
       in B.Dyn_Array.app1 (d, n)
       end

  fun test_data conn =
       let val (outgoing, sender) = (Snow_Ip.allocate_send (conn, 60))
       in fill (outgoing, 60);
	  sender
       end

  fun test_status conn =
       let val data = B.Dyn_Array.init1 (60, FoxWord8.intToWord)
	   val _ = B.Dyn_Array.update1 (data, 0, SW.n8 "0x45")
	   (* Put the ip destination address in the right place. *)
	   val _ = B.Dyn_Array.update4 (data, 16, snow);
	   val _ = B.Dyn_Array.update1 (data, 9, SW.n8 "0");
	   val allocation =
	        Snow_Icmp.Out.Parameter_Problem {pointer = SW.n8 "0",
						 data = data}
	   val (_, sender) = Snow_Icmp.allocate_send (conn, allocation)
       in sender
       end

  fun test_ipicmp_data_and_status () =
       (test_ipicmp ("data_and_status", 
		     Data test_data,
		     "source 128.2.222.84 data 1ux00.1ux01.1ux02.1ux03." ^
		     "1ux04.1ux05.1ux06.1ux07.1ux08.1ux09.1ux0a.1ux0b." ^
		     "1ux0c.1ux0d.1ux0e.1ux0f.1ux10.1ux11.1ux12.1ux13." ^
		     "1ux14.1ux15.1ux16.1ux17.1ux18.1ux19.1ux1a.1ux1b." ^
		     "1ux1c.1ux1d.1ux1e.1ux1f.1ux20.1ux21.1ux22.1ux23." ^
		     "1ux24.1ux25.1ux26.1ux27.1ux28.1ux29.1ux2a.1ux2b." ^
		     "1ux2c.1ux2d.1ux2e.1ux2f.1ux30.1ux31.1ux32.1ux33." ^
		     "1ux34.1ux35.1ux36.1ux37.1ux38.1ux39.1ux3a.1ux3b");
       test_ipicmp ("status/active", 
		    Status test_status,
		    "Icmp_Specific parameter problem: header problem " ^
		    "at position 0"))

(*
		3.	test_ipicmp_passive
*)

  fun test_ipicmp_passive (name, datastatus, expected) =
       let fun compare_data (expected_data, p) () = 
	        (debug_print (fn _ => "compare_status got data packet");
		 if Quick_Ip.makestring_incoming (p, NONE) = expected then
		  true
		 else
		  (debug_print (fn _ => "error in test_icmp_run: received \"" ^
				Quick_Ip.makestring_incoming (p, NONE) ^
				"\", expected \"" ^ expected ^ "\"");
                   case datastatus of
		      Data _ => ()
		    | Status _ =>
		       debug_print (fn _ => "expected status, got data.");
                   false))
	   fun compare_status (expected_status, p) () = 
	        (debug_print (fn _ => "compare_status got status packet");
		 if Quick_Ip.makestring_status p = expected then
		  true
		 else
		  (debug_print (fn _ => "error in test_icmp_run: received \"" ^
				Quick_Ip.makestring_status p ^
				"\", expected \"" ^ expected ^ "\"");
		   case datastatus of
                      Data _ =>
		       debug_print (fn _ => "expected data, got status.")
		    | Status _ => ();
                   false))
	   fun test_receive connection =
	        (fn p => (B.Test.test (name,(compare_data (expected, p)))),
		 fn p => (B.Test.test (name,(compare_status (expected, p)))))
	   fun ignore_handler _ = (fn _ => (), fn _ => ())
	   val _ = (debug_print (fn _ => "About to initialize quick");
		    Quick_Ip.initialize ();
		    debug_print (fn _ => "About to initialize snow");
		    Snow_Ip.initialize ();
		    debug_print (fn _ => "Done initializing snow"))
	   val _ = debug_print (fn _ =>
				"Quick trying to passively connect to snow")
	   val (quick_ip_connection_to_snow_stop,
		quick_ip_connection_to_snow_connections) =
	         Quick_Ip.start_passive (quick_ip_address_for_snow,
					 Quick_Ip.Handler test_receive, SOME 1)
	   val _ = debug_print (fn _ => "Snow trying to icmp connect to quick")
	   val snow_icmp_connection_to_quick = 
	        Snow_Icmp.connect (snow_icmp_address_for_quick,
				   Snow_Icmp.Handler ignore_handler)
	   val _ = debug_print (fn _ => "Snow trying to ip connect to quick")
	   val snow_ip_connection_to_quick = 
	        Snow_Ip.connect (snow_ip_address_for_quick,
				 Snow_Ip.Handler ignore_handler)
	   val sleep_time = 1000
       in debug_print (fn _ => "in test_ipicmp_passive");
          case datastatus of
             Data f => (f snow_ip_connection_to_quick) ()
           | Status f => (f snow_icmp_connection_to_quick)();
          B.Scheduler.sleep sleep_time;
	  quick_ip_connection_to_snow_stop();
	  map Quick_Ip.close (quick_ip_connection_to_snow_connections());
	  Snow_Ip.close snow_ip_connection_to_quick;
	  Snow_Icmp.close snow_icmp_connection_to_quick;
	  Quick_Ip.finalize ();
	  Snow_Ip.finalize ();
	  ()
       end (* let *)

  fun test_passive_data conn =
       let val (outgoing,sender) = (Snow_Ip.allocate_send (conn, 60))
       in fill (outgoing, 60);
	  sender
       end

  fun test_passive_status conn =
       let val data = B.Dyn_Array.init1 (60, FoxWord8.intToWord)
	   val _ = B.Dyn_Array.update1 (data, 0, SW.n8 "0x45")
	   (* Put the ip destination address in the right place. *)
	   val _ = B.Dyn_Array.update4 (data, 16, snow);
	   val _ = B.Dyn_Array.update1 (data, 9, SW.n8 "0");
	   val allocation =
	        Snow_Icmp.Out.Parameter_Problem {pointer = SW.n8 "0",
						 data = data}
	   val (_, sender) = Snow_Icmp.allocate_send (conn, allocation)
       in sender
       end

  fun test_ipicmp_passive_data_and_status () =
       (test_ipicmp_passive ("passive_data_and_status", 
			     Data test_passive_data,
			     "source 128.2.222.84 data 1ux00.1ux01.1ux02." ^
			     "1ux03.1ux04.1ux05.1ux06.1ux07.1ux08.1ux09." ^
			     "1ux0a.1ux0b.1ux0c.1ux0d.1ux0e.1ux0f.1ux10." ^
			     "1ux11.1ux12.1ux13.1ux14.1ux15.1ux16.1ux17." ^
			     "1ux18.1ux19.1ux1a.1ux1b.1ux1c.1ux1d.1ux1e." ^
			     "1ux1f.1ux20.1ux21.1ux22.1ux23.1ux24.1ux25." ^
			     "1ux26.1ux27.1ux28.1ux29.1ux2a.1ux2b.1ux2c." ^
			     "1ux2d.1ux2e.1ux2f.1ux30.1ux31.1ux32.1ux33." ^
			     "1ux34.1ux35.1ux36.1ux37.1ux38.1ux39.1ux3a." ^
			     "1ux3b");
   (* I should get nothing here as status's don't open connections. *)
       test_ipicmp_passive ("status/passive",
			    Status test_passive_status,
			    ""))

(*
		5.	run_tests
*)

  fun run_tests () =
       (test_ipicmp_data_and_status ();
	test_ipicmp_passive_data_and_status ())

  fun run () =
       (B.Scheduler.reset ();
	init_stacks ();
	B.Test.tests ("Ip over Icmp", 3, fn _ => () (* run_tests *) );
	fin_stacks ())

  val _ = if ! B.Debug.do_tests then run () else ()

 end (* struct *)

structure Test_Ip_Icmp = Test_Ip_Icmp (structure B =  Fox_Basis
                                       val debug_level = NONE)
